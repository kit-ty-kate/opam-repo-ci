open Current.Syntax
open Opam_repo_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Common = Opam_repo_ci_api.Common

let master_distro = Dockerfile_distro.resolve_alias Dockerfile_distro.master_distro
let default_compiler = Ocaml_version.with_just_major_and_minor Ocaml_version.Releases.latest

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "http://147.75.80.95/github/%s/%s/commit/%s" owner name hash)

let github_status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg _)    -> Github.Api.Status.v ~url `Failure ~description:"Failed"

let set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  let repo = Github.Api.Repo.id repo in
  Index.set_active_refs ~repo (
    xs |> List.map @@ fun x ->
    let commit = Github.Api.Commit.id x in
    let gref = Git.Commit_id.gref commit in
    let hash = Git.Commit_id.hash commit in
    (gref, hash)
  );
  xs

module OpamPackage = struct
  include OpamPackage
  let pp = Fmt.of_to_string to_string
end

let with_label l t =
  Current.component "%s" l |>
  let> v = t in
  Current.Primitive.const v

(* [dep_list_map] is like [Current.list_map], except that the output is fixed to the empty list until
   the input is successful. You must ensure that the status of the input is reported elsewhere. *)
let dep_list_map (type a) (module M : Current_term.S.ORDERED with type t = a) ?collapse_key f input =
  let results = Current.list_map ?collapse_key (module M) f input in
  let+ state = Current.state ~hidden:true results
  and+ input = Current.state ~hidden:true input
  in
  match input, state with
  | Error _, _ -> []
  | Ok _, Ok x -> x
  (* The results of a [dep_list_map] are nodes, so they should always be ready and successful. *)
  | Ok _, Error (`Msg m) -> failwith m
  | Ok _, Error (`Active _) ->
    Logs.warn (fun f -> f "dep_list_map: input is ready but output is pending!");
    []

let build_spec ~platform ?revdep pkg =
  let+ revdep = Current.option_seq revdep
  and+ pkg = pkg
  in
  Build.Spec.opam ~platform ?revdep ~with_tests:false pkg

let test_spec ~platform ~after ?revdep pkg =
  let+ revdep = Current.option_seq revdep
  and+ _ = after
  and+ pkg = pkg
  in
  Build.Spec.opam ~platform ?revdep ~with_tests:true pkg

module Revdep = struct
  module Map = OpamPackage.Map

  type options = {
    with_tests : bool;
  }

  type t = (OpamPackage.t * options)

  let compare (pkg1, {with_tests}) (pkg2, options) =
    compare ((pkg1, with_tests) : OpamPackage.t * bool) (pkg2, options.with_tests)

  let pp =
    Fmt.of_to_string (fun (pkg, {with_tests = _}) ->
      Fmt.strf "%s" (OpamPackage.to_string pkg)
    )
end

let combine_revdeps ~revdeps ~revdeps_with_tests =
  let+ revdeps = revdeps
  and+ revdeps_with_tests = revdeps_with_tests in
  let map =
    List.fold_left (fun map pkg -> Revdep.Map.add pkg {Revdep.with_tests = false} map)
      Revdep.Map.empty revdeps
  in
  let map =
    List.fold_left (fun map pkg -> Revdep.Map.add pkg {Revdep.with_tests = true} map)
      map revdeps_with_tests
  in
  Revdep.Map.bindings map

let build_tests ~revdep ~ocluster ~master ~base ~platform ~pkg ~after source =
  let tests =
    let spec = test_spec ~platform ?revdep pkg ~after in
    Build.v ocluster ~label:"test" ~base ~spec ~master source
  in
  let+ state = Current.state ~hidden:true after
  and+ tests = Node.action `Built tests in
  match state with
  | Error _ -> []
  | Ok _ -> [Node.leaf ~label:"tests" tests]

(* List the revdeps of [pkg] (using [builder] and [image]) and test each one
   (using [spec] and [base], merging [source] into [master]). *)
let test_revdeps ~ocluster ~master ~base ~platform ~pkg ~after:main_build source =
  let revdeps = Build.list_revdeps ~base ocluster ~with_tests:false ~platform ~pkg ~master source in
  let revdeps_with_tests = Build.list_revdeps ~base ocluster ~with_tests:true ~platform ~pkg ~master source in
  let+ state = Current.state ~hidden:true main_build
  and+ tests =
    let revdeps = combine_revdeps ~revdeps ~revdeps_with_tests in
    revdeps
    |> dep_list_map (module Revdep) (fun revdep ->
        let with_tests = Current.map (fun (_, {Revdep.with_tests}) -> with_tests) revdep in
        let revdep = Current.map (fun (pkg, _) -> pkg) revdep in
        let image =
          let spec = build_spec ~platform ~revdep pkg in
          Build.v ocluster ~label:"build" ~base ~spec ~master source
        in
        let+ label = Current.map OpamPackage.to_string revdep
        and+ build = Node.action `Built image
        and+ tests = build_tests ~revdep:(Some revdep) ~ocluster ~master ~base ~platform ~pkg ~after:image source
        and+ with_tests = with_tests
        in
        if with_tests then
          Node.actioned_branch ~label build tests
        else
          Node.leaf ~label build
      )
  in
  match state with
  | Error _ -> []
  | Ok _ -> [Node.branch ~label:"revdeps" tests]

let build_with_cluster ~ocluster ~analysis ~master source =
  let pkgs = Current.map Analyse.Analysis.packages analysis in
  let build ~pool ~revdeps label variant =
    let platform = {Platform.label; pool; variant} in
    let analysis = with_label variant analysis in
    let pkgs =
      (* Add fake dependency from pkgs to analysis so that the package being tested appears
         below the platform, to make the diagram look nicer. Ideally, the pulls of the
         base images should be moved to the top (not be per-package at all). *)
      let+ _ = analysis
      and+ pkgs = pkgs in
      pkgs
    in
    pkgs |> dep_list_map ~collapse_key:"pkg" (module OpamPackage) (fun pkg ->
        let base =
          let+ repo_id = Docker.peek ~schedule:weekly ~arch:"amd64" ("ocaml/opam:" ^ variant) in
          Current_docker.Raw.Image.of_hash repo_id
        in
        let image =
          let spec = build_spec ~platform pkg in
          Build.v ocluster ~label:"build" ~base ~spec ~master source in
        let+ pkg = pkg
        and+ build = Node.action `Built image
        and+ tests = build_tests ~revdep:None ~ocluster ~master ~base ~platform ~pkg ~after:image source
        and+ revdeps =
          if revdeps then test_revdeps ~ocluster ~master ~base ~platform ~pkg source ~after:image
          else Current.return []
        in
        let label = OpamPackage.to_string pkg in
        Node.actioned_branch ~label build (tests @ revdeps)
      )
    |> Current.map (Node.branch ~label)
    |> Current.collapse ~key:"platform" ~value:label ~input:analysis
  in
  let build = build ~pool:"linux-x86_64" in
  let+ analysis = Node.action `Checked analysis
  and+ compilers =
    Current.list_seq begin
      let master_distro = Dockerfile_distro.tag_of_distro master_distro in
      (Ocaml_version.Releases.recent @ Ocaml_version.Releases.unreleased_betas) |>
      List.map (fun v ->
        let v = Ocaml_version.with_just_major_and_minor v in
        let revdeps = Ocaml_version.equal v default_compiler in (* TODO: Remove this when the cluster is ready *)
        let v = Ocaml_version.to_string v in
        build ~revdeps v @@ master_distro^"-ocaml-"^v
      )
    end
  and+ distributions =
    Current.list_seq begin
      let default_compiler = Ocaml_version.to_string default_compiler in
      (Dockerfile_distro.active_tier1_distros `X86_64 @ Dockerfile_distro.active_tier2_distros `X86_64) |>
      List.fold_left (fun acc distro ->
        if Dockerfile_distro.compare distro master_distro = 0 then (* TODO: Add Dockerfile_distro.equal *)
          acc
        else
          let distro = Dockerfile_distro.tag_of_distro distro in
          build ~revdeps:false distro (distro^"-ocaml-"^default_compiler) :: acc
      ) []
    end
  and+ extras =
    let master_distro = Dockerfile_distro.tag_of_distro master_distro in
    let default_compiler = Ocaml_version.to_string default_compiler in
    Current.list_seq [
      build ~revdeps:false "flambda" @@ master_distro^"-ocaml-"^default_compiler^"-flambda";
    ]
  in
  Node.root [
    Node.leaf ~label:"(analysis)" analysis;
    Node.branch ~label:"compilers" compilers;
    Node.branch ~label:"distributions" distributions;
    Node.branch ~label:"extras" extras;
  ]

let list_errors ~ok errs =
  let groups =  (* Group by error message *)
    List.sort compare errs |> List.fold_left (fun acc (msg, l) ->
        match acc with
        | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
        | _ -> (msg, [l]) :: acc
      ) []
  in
  Error (`Msg (
      match groups with
      | [] -> "No builds at all!"
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") pp_label) errs
    ))

let summarise results =
  results
  |> Node.flatten (fun ~label ~job_id:_ ~result -> (label, result))
  |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok ()                     (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)

let get_prs repo =
  let refs =
    Current.component "Get PRs" |>
    let> (api, repo) = repo in
    Github.Api.refs api repo
  in
  let master =
    refs
    |> Current.map (Github.Api.Ref_map.find (`Ref "refs/heads/master"))
    |> Current.map Github.Api.Commit.id
    |> with_label "master"
    |> Git.fetch
  in
  let prs =
    let+ refs = refs in
    Github.Api.Ref_map.fold begin fun key head acc ->
      match key with
      | `Ref _ -> acc (* Skip branches, only check PRs *)
      | `PR _ -> head :: acc
    end refs []
  in
  master, prs

let test_repo ~ocluster ~push_status repo =
  let master, prs = get_prs repo in
  let prs = set_active_refs ~repo prs in
  prs |> Current.list_iter ~collapse_key:"pr" (module Github.Api.Commit) @@ fun head ->
  let commit_id = Current.map Github.Api.Commit.id head in
  let src = Git.fetch commit_id in
  let analysis = Analyse.examine ~master src in
  let builds = build_with_cluster ~ocluster ~analysis ~master commit_id in
  let summary = Current.map summarise builds in
  let status =
    let+ summary = summary in
    match summary with
    | Ok () -> `Passed
    | Error (`Active `Running) -> `Pending
    | Error (`Msg _) -> `Failed
  in
  let index =
    let+ commit = head
    and+ jobs = Current.map (Node.flatten (fun ~label ~job_id ~result:_ -> (label, job_id))) builds
    and+ status = status in
    let repo = Current_github.Api.Commit.repo_id commit in
    let hash = Current_github.Api.Commit.hash commit in
    Index.record ~repo ~hash ~status jobs
  and set_github_status =
    summary
    |> github_status_of_state ~head
    |> (if push_status then Github.Api.Commit.set_status head "opam-ci"
        else Current.ignore_value)
  in
  Current.all [index; set_github_status]

let local_test ~ocluster repo () =
  let ocluster = Build.config ~timeout:Conf.build_timeout ocluster in
  test_repo ~ocluster ~push_status:false (Current.return repo)

let v ~ocluster ~app () =
  let ocluster = Build.config ~timeout:Conf.build_timeout ocluster in
  Github.App.installations app |> Current.list_iter (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter (module Github.Api.Repo) @@ fun repo ->
  test_repo ~ocluster ~push_status:(Conf.profile = `Production) repo

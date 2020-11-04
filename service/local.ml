(* Utility program for testing the CI pipeline on a local repository. *)

open Lwt.Infix

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Logging.init ();
  Mirage_crypto_rng_unix.initialize ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let run_capnp = function
  | None -> Lwt.return (Capnp_rpc_unix.client_only_vat (), None)
  | Some public_address ->
    let config =
      Capnp_rpc_unix.Vat_config.create
        ~public_address
        ~secret_key:(`File Conf.Capnp.secret_key)
        (Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port)
    in
    let rpc_engine, rpc_engine_resolver = Capnp_rpc_lwt.Capability.promise () in
    let service_id = Capnp_rpc_unix.Vat_config.derived_id config "ci" in
    let restore = Capnp_rpc_net.Restorer.single service_id rpc_engine in
    Capnp_rpc_unix.serve config ~restore >>= fun vat ->
    Capnp_rpc_unix.Cap_file.save_service vat service_id Conf.Capnp.cap_file |> or_die;
    Logs.app (fun f -> f "Wrote capability reference to %S" Conf.Capnp.cap_file);
    Lwt.return (vat, Some rpc_engine_resolver)

let main config mode capnp_address submission_uri repo =
  Logging.run begin
    run_capnp capnp_address >>= fun (vat, rpc_engine_resolver) ->
    let ocluster = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
    let repo = Current_git.Local.v (Fpath.v repo) in
    let engine = Current.Engine.create ~config (Pipeline.local_test ~ocluster repo) in
    rpc_engine_resolver |> Option.iter (fun r -> Capnp_rpc_lwt.Capability.resolve_ok r (Api_impl.make_ci ~engine));
    let routes = Current_web.routes engine in
    let site = Current_web.Site.(v ~has_role:allow_all) ~name:"opam-repo-ci-local" routes in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let capnp_address =
  Arg.value @@
  Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None @@
  Arg.info
    ~doc:"Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no RPC)"
    ~docv:"ADDR"
    ["capnp-address"]

let submission_service =
  Arg.required @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE"
    ["submission-service"]

let cmd =
  let doc = "Test opam-repo-ci on a local Git clone" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $
        capnp_address $ submission_service $ repo),
  Term.info "opam-repo-ci-local" ~doc

let () = Term.(exit @@ eval cmd)

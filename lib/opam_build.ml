let download_cache = "opam-archives"
let cache = [ Obuilder_spec.Cache.v download_cache ~target:"/home/opam/.opam/download-cache" ]
let network = ["host"]

let opam_install ~pin ~with_tests ~pkg =
  let pkg = OpamPackage.to_string pkg in
  let open Obuilder_spec in
  let pin =
    if pin then
      let version =
        let idx = String.index pkg '.' + 1 in
        String.sub pkg idx (String.length pkg - idx)
      in
      [ run "opam pin add -k version -yn %s %s" pkg version ]
    else
      []
  in
  pin @ [
    run ~cache "opam remove -y %s" pkg;
    (* TODO: Replace by two calls to opam install + opam install -t using the OPAMDROPINSTALLEDPACKAGES feature *)
    run ~cache ~network "opam depext -uivy%s %s" (if with_tests then "t" else "") pkg
  ]

let setup_repository ~variant ~compiler =
  let open Obuilder_spec in
  let switch_setup =
    match compiler.Platform.compiler_option with
    | `System ->
        let full_v = Ocaml_version.to_string compiler.Platform.compiler_full_version in
        [
          run "opam source 'ocaml-base-compiler.%s' && \
               cd 'ocaml-base-compiler.%s' && \
               ./configure -prefix /usr/local --with-debug-runtime && \
               make world.opt && \
               sudo make install && \
               cd .. && \
               rm -rf 'ocaml-base-compiler.%s'"
            full_v full_v full_v;
          run "opam switch create ocaml-system";
        ]
    | `Default | `Flambda ->
        []
  in
  let opam_extras =
    if Ocaml_version.compare compiler.Platform.compiler_version Ocaml_version.Releases.v4_08 < 0 then
      [ run ~cache ~network "opam install -y ocaml-secondary-compiler" ] (* Speed up builds for dune >= 2.0 *)
    else
      []
  in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" variant then
      [ run ~network "sudo dnf install -y findutils" ] (* (we need xargs) *)
    else
      []
  in
  env "OPAMDOWNLOADJOBS" "1" :: (* Try to avoid github spam detection *)
  env "OPAMERRLOGLEN" "0" :: (* Show the whole log if it fails *)
  env "OPAMSOLVERTIMEOUT" "500" :: (* Increase timeout. Poor mccs is doing its best *)
  env "OPAMPRECISETRACKING" "1" :: (* Mitigate https://github.com/ocaml/opam/issues/3997 *)
  user ~uid:1000 ~gid:1000 :: switch_setup @ distro_extras @ opam_extras @ [
    copy ["."] ~dst:"/src/";
    run "opam repository set-url --strict default file:///src";
  ]

let spec ~base ~variant ~revdep ~compiler ~with_tests ~pkg =
  let open Obuilder_spec in
  let revdep = match revdep with
    | None -> []
    | Some revdep -> opam_install ~pin:false ~with_tests:false ~pkg:revdep
  and tests = match with_tests, revdep with
    | true, None -> opam_install ~pin:false ~with_tests:true ~pkg
    | true, Some revdep -> opam_install ~pin:false ~with_tests:true ~pkg:revdep
    | false, _ -> []
  in
  { from = base;
    ops =
      setup_repository ~variant ~compiler
      @ opam_install ~pin:true ~with_tests:false ~pkg
      @ revdep
      @ tests
  }

let revdeps ~with_tests ~base ~variant ~compiler ~pkg =
  let open Obuilder_spec in
  let pkg = Filename.quote (OpamPackage.to_string pkg) in
  let with_tests = if with_tests then " --with-test" else "" in
  { from = base;
    ops =
      setup_repository ~variant ~compiler
      @ [
        run "echo '@@@OUTPUT' && \
             opam list -s --color=never --depends-on %s --coinstallable-with %s --installable --all-versions --depopts%s && \
             echo '@@@OUTPUT'"
          pkg pkg with_tests
      ]
  }

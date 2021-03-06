let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

let opam_install ~pin ~with_tests ~for_user ~pkg =
  let download_cache_prefix = if for_user then "" else download_cache ^ " " in
  let open Dockerfile in
  let pin =
    if pin then
      let version =
        let idx = String.index pkg '.' + 1 in
        String.sub pkg idx (String.length pkg - idx)
      in
      run "opam pin add -k version -yn %s %s" pkg version
    else
      empty
  in
  pin @@
  run "%sopam depext -uivy%s %s" download_cache_prefix (if with_tests then "t" else "") pkg

let dockerfile ~base ~variant ~revdep ~with_tests ~pkg ~for_user =
  let open Dockerfile in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" variant then
      run "sudo dnf install -y findutils" (* (we need xargs) *)
    else
      empty
  in
  let opam_extras =
    if Astring.String.is_suffix ~affix:"-ocaml-4.06" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.05" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.04" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.03" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.02" variant then
      run "%s opam install -y ocaml-secondary-compiler" download_cache (* Speed up builds for dune >= 2.0 *)
    else
      empty
  in
  let revdep = match revdep with
    | None -> empty
    | Some revdep -> opam_install ~pin:false ~with_tests:false ~pkg:revdep ~for_user
  and tests = match with_tests, revdep with
    | true, None -> opam_install ~pin:false ~with_tests:true ~pkg ~for_user
    | true, Some revdep -> opam_install ~pin:false ~with_tests:true ~pkg:revdep ~for_user
    | false, _ -> empty
  in
  (if for_user then empty
   else comment "syntax = docker/dockerfile:experimental@sha256:ee85655c57140bd20a5ebc3bb802e7410ee9ac47ca92b193ed0ab17485024fe5") @@
  from base @@
  distro_extras @@
  opam_extras @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  run "opam repository set-url --strict default file:///src" @@
  opam_install ~pin:true ~with_tests:false ~pkg ~for_user @@
  revdep @@
  tests

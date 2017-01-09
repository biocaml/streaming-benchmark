open Printf
open Solvuu_build.Std
open Solvuu_build.Util

let project_name = "streaming_benchmark"
let version = "dev"

let annot = ()
let bin_annot = ()
let g = ()
let short_paths = ()
let thread = ()
let w = "A-4-33-41-42-44-45-48"

let lib ?findlib_deps ?internal_deps ?ml_files lib_name
  : Project.item
  =
  Project.lib (sprintf "%s_%s" project_name lib_name)
    ~annot ~bin_annot ~g ~short_paths ~thread ~w
    ~pkg:(sprintf "%s.%s" project_name lib_name)
    ~dir:(sprintf "lib/%s" lib_name)
    ~style:(`Pack (sprintf "%s_%s" project_name lib_name))
    ~build_plugin:false (* solvuu-build doesn't implement plugin
                           compilation in case there are C files,
                           which is the case of biocaml_unix. Since
                           most other libs depend on it, we simply
                           refrain from compiling plugins for now.  *)
    ?findlib_deps
    ?internal_deps
    ?ml_files

let app ?internal_deps name : Project.item =
  Project.app name
    ~annot ~bin_annot ~g ~short_paths ~thread ~w
    ~file:(sprintf "app/%s.ml" name)
    ?internal_deps

let unix = lib "unix" ~findlib_deps:[
    "angstrom.unix" ;
    "biocaml.base" ;
    "biocaml.unix" ;
    "containers" ;
    "core_bench" ;
    "sosa"
  ]

let streaming_benchmark = app "streaming_benchmark"
    ~internal_deps:[unix]

let () =
  Project.basic1
    ~project_name
    ~version
    [ unix ; streaming_benchmark ]

open Core.Std
open CFStream

let gc = String.count ~f:(function
    | 'c' | 'G' | 'C' | 'g' -> true
    | _ -> false
  )

let gc_accu (k, n) s =
  k + gc s,
  n + String.length s

let print_result (k, n) =
  printf "%f\n%!" (float k /. float n)

let biocaml_unix fn =
  let open Biocaml_unix.Std in
  Fasta.with_file fn ~f:(fun _ items ->
      Stream.Result.fold' items ~init:(0,0) ~f:(fun accu item ->
          gc_accu accu item.Fasta.sequence
        )
    )
  |> ok_exn
  |> print_result

let main mode fn () = match mode with
  | "biocaml-unix" -> biocaml_unix fn
  | _ -> failwithf "Unknown mode %s" mode ()

let command =
  let open Command in
  basic ~summary:"Computes GC in FASTA file"
    Spec.(
      step (fun k mode fn -> k mode fn)
      +> flag "mode" (required string) ~doc:"{biocaml-unix} Choose implementation"
      +> anon ("INPUT-FILE" %: string)
    )
    main

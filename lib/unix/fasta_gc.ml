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
  printf "%d %d %f\n%!" k n (float k /. float n)

let fold_file fn ~init ~f =
  let n = 32768 in
  let buf = String.create n in
  In_channel.with_file fn ~f:(fun ic ->
      let rec loop accu =
        match input ic buf 0 n with
        | 0 -> f accu None
        | len ->
          Some (String.sub buf ~pos:0 ~len)
          |> f accu
          |> loop

      in
      loop init
    )

let biocaml_base fn =
  let open Biocaml_base.Std in
  fold_file fn
    ~init:((0, 0), Fasta.Parser0.initial_state ())
    ~f:(fun (accu, state) input ->
        match Fasta.Parser0.step state input with
        | Ok (state', items) ->
          let accu' = List.fold items ~init:accu ~f:(fun accu ->
              function
              | `Comment _ | `Description _ | `Empty_line -> accu
              | `Partial_sequence s -> gc_accu accu s
            )
          in
          accu', state'

        | Error _ -> failwith "Biocaml_base.Fasta.Parser0 error"
      )
  |> fst
  |> print_result


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
  | "biocaml-base" -> biocaml_base fn
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

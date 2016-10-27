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

let biocaml_base0 fn =
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

let biocaml_base fn =
  let open Biocaml_base.Std in
  fold_file fn
    ~init:((0, 0), Fasta.Parser.initial_state ())
    ~f:(fun (accu, state) input ->
        match Fasta.Parser.step state input with
        | Ok (state', items) ->
          let accu' = List.fold items ~init:accu ~f:(fun accu item ->
              gc_accu accu item.Fasta.sequence
            )
          in
          accu', state'

        | Error _ -> failwith "Biocaml_base.Fasta.Parser0 error"
      )
  |> fst
  |> print_result

let biocaml_unix0 fn =
  let open Biocaml_unix.Std in
  In_channel.with_file fn ~f:(fun ic ->
      Fasta.read0 ic
      |> Stream.Result.fold' ~init:(0,0) ~f:(fun accu item ->
          match item with
          | `Comment _ | `Description _ | `Empty_line -> accu
          | `Partial_sequence s -> gc_accu accu s
        )
      |> ok_exn
      |> print_result
    )


let biocaml_unix fn =
  let open Biocaml_unix.Std in
  Fasta.with_file fn ~f:(fun _ items ->
      Stream.Result.fold' items ~init:(0,0) ~f:(fun accu item ->
          gc_accu accu item.Fasta.sequence
        )
    )
  |> ok_exn
  |> print_result

let mean x =
  Array.fold x ~init:0. ~f:( +. )
  /. float (Array.length x)

let sd x =
  let mu = mean x in
  sqrt (mean (Array.map x ~f:(fun x -> (x -. mu) ** 2.)))

let time ~n f x =
  let repetition _ =
    let t1 = Time.now () in
    f x ;
    let t2 = Time.now () in
    Time.Span.to_float (Time.diff t2 t1)
  in
  let repetitions = Array.init n repetition in
  mean repetitions,
  sd repetitions

let main fn () =
  let n = 10 in
  let compute (name, f) =
    let mu, sigma = time ~n f fn in
    name, mu, sigma
  in
  let render (name, mu, sigma) =
    let delta = sigma *. 1.96 /. sqrt (float n) in
    (* improper CI, should use student's law *)
    let lo, hi = mu -. delta, mu +. delta in
    printf "%s\t%.3g [%.3g, %.3g]\n%!" name mu lo hi
  in
  let bench = [
    "biocaml-unix0", biocaml_unix0 ;
    "biocaml-unix", biocaml_unix ;
    "biocaml-base0", biocaml_base0 ;
    "biocaml-base", biocaml_base ;
  ]
  in
  List.map bench ~f:compute
  |> List.iter ~f:render

let command =
  let open Command in
  basic ~summary:"Computes GC in FASTA file"
    Spec.(
      step (fun k mode fn -> k mode fn)
      +> anon ("INPUT-FILE" %: string)
    )
    main

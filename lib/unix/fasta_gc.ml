open Core
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
        match In_channel.input ic buf 0 n with
        | 0 -> f accu None
        | len ->
          Some (String.sub buf ~pos:0 ~len)
          |> f accu
          |> loop

      in
      loop init
    )

let biocaml_base0 fn =
  let open Biocaml_base in
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

let biocaml_base fn =
  let open Biocaml_base in
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

let biocaml_unix0 fn =
  let open Biocaml_unix in
  In_channel.with_file fn ~f:(fun ic ->
      Fasta.read0 ic
      |> Stream.Result.fold' ~init:(0,0) ~f:(fun accu item ->
          match item with
          | `Comment _ | `Description _ | `Empty_line -> accu
          | `Partial_sequence s -> gc_accu accu s
        )
      |> ok_exn
    )


let biocaml_unix fn =
  let open Biocaml_unix in
  Fasta.with_file fn ~f:(fun _ items ->
      Stream.Result.fold' items ~init:(0,0) ~f:(fun accu item ->
          gc_accu accu item.Fasta.sequence
        )
    )
  |> ok_exn


let angstrom_parser =
  let open Angstrom in
  let not_eol c = c <> '\n' in
  let rec item acc =
    char '>'
    *> skip_while not_eol
    *> sequence acc

  and sequence acc =
    take_while not_eol >>= fun s ->
    end_of_line *>
    peek_char >>= function
    | None -> return acc
    | Some '>' -> item acc
    | Some c -> sequence (gc_accu acc s)
  in
  item (0, 0)

let angstrom fn =
  In_channel.with_file fn ~f:(fun ic ->
      (* Angstrom_unix.parse (count_chars ()) ic *)
      Angstrom_unix.parse angstrom_parser ic
    )
  |> snd
  |> (
    function
      Pervasives.Ok r -> r
    | Pervasives.Error e -> failwith e
  )

let mean x =
  Array.fold x ~init:0. ~f:( +. )
  /. float (Array.length x)

let sd x =
  let mu = mean x in
  sqrt (mean (Array.map x ~f:(fun x -> (x -. mu) ** 2.)))

type 'a perf = {
  time : 'a ;
  minor_words : 'a ;
  promoted_words : 'a ;
  major_words : 'a ;
}

let bench ~verbose ~n f x =
  let repetition _ =
    Gc.full_major () ;
    let stats1 = Gc.quick_stat () in
    let t1 = Time.now () in
    let r = f x in
    let t2 = Time.now () in
    let stats2 = Gc.quick_stat () in
    if verbose then print_result r ;
    {
      time = Time.Span.to_proportional_float (Time.diff t2 t1) ;
      minor_words = stats2.Gc.Stat.minor_words -. stats1.Gc.Stat.minor_words ;
      promoted_words = stats2.Gc.Stat.promoted_words -. stats1.Gc.Stat.promoted_words ;
      major_words = stats2.Gc.Stat.major_words -. stats1.Gc.Stat.major_words ;
    }
  in
  let repetitions = Array.init n ~f:repetition in
  let time = Array.map repetitions ~f:(fun x -> x.time) in
  let minor_words = Array.map repetitions ~f:(fun x -> x.minor_words) in
  let promoted_words = Array.map repetitions ~f:(fun x -> x.promoted_words) in
  let major_words = Array.map repetitions ~f:(fun x -> x.major_words) in
  {
    time = mean time, sd time ;
    minor_words = mean minor_words, sd minor_words ;
    promoted_words = mean promoted_words, sd promoted_words ;
    major_words = mean major_words, sd major_words ;
  }

let main verbose fn () =
  let n = 10 in
  let compute (name, f) =
    let res = bench ~verbose ~n f fn in
    name, res.time, res.minor_words, res.promoted_words, res.major_words
  in
  let render_obs (mu, sigma) =
    let delta = sigma *. 1.96 /. sqrt (float n) in
    (* improper CI, should use student's law *)
    let lo, hi = mu -. delta, mu +. delta in
    sprintf "%.3f [%.3f, %.3f]" mu lo hi
  in
  let render (name, time, minor_words, promoted_words, major_words) =
    printf "%s\t%s\t%s\t%s\t%s\n%!"
      name
      (render_obs time)
      (render_obs minor_words)
      (render_obs promoted_words)
      (render_obs major_words)
  in
  let bench = [
    "angstrom", angstrom ;
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
      +> flag "--verbose" no_arg ~doc:" prints more stuff"
      +> anon ("INPUT-FILE" %: string)
    )
    main

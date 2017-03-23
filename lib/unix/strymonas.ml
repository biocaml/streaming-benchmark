module Cruanes_sequence = Sequence
open Core.Std
open Core_bench.Std

let int_list = List.range 0 1_000_000
let square = (fun x -> x * x)

module Cruanes_sequence_impl = struct
  module S = Cruanes_sequence

  let int_sum () =
    S.of_list int_list
    |> S.fold ( + ) 0
    |> ignore

  let int_sumOfSquares () =
    S.of_list int_list
    |> S.map square
    |> S.fold ( + ) 0
    |> ignore
end

module Cseq_impl = struct
  let int_sum () =
    Cseq.of_list int_list
    |> Cseq.fold_left ( + ) 0
    |> ignore

  let int_sumOfSquares () =
    Cseq.of_list int_list
    |> Cseq.map square
    |> Cseq.fold_left ( + ) 0
    |> ignore
end

module Enum_impl = struct
  open Batteries

  let int_sum () =
    List.enum int_list
    |> Enum.fold ( + ) 0
    |> ignore

  let int_sumOfSquares () =
    List.enum int_list
    |> Enum.map square
    |> Enum.fold ( + ) 0
    |> ignore
end

module Gen_impl = struct
  open Gen

  let int_sum () =
    Gen.of_list int_list
    |> Gen.fold ( + ) 0

  let int_sumOfSquares () =
    Gen.of_list int_list
    |> Gen.map square
    |> Gen.fold ( + ) 0
end

module Imp_impl = struct
  let int_sum () =
    let l = ref int_list in
    let sum = ref 0 in
    while !l <> [] do
      sum := !sum + List.hd_exn !l ;
      l := List.tl_exn !l
    done

  let int_sumOfSquares () =
    let l = ref int_list in
    let sum = ref 0 in
    while !l <> [] do
      let n = List.hd_exn !l in
      sum := !sum + n * n ;
      l := List.tl_exn !l
    done
end

module Pipes_impl = struct
  open Pipes_unix.Pipe

  let int_sum () =
    run (
      from_list int_list
      $$ fold 0 ( + )
    )
    |> ignore

  let int_sumOfSquares () =
    run (
      from_list int_list
      $$ map square
      $$ fold 0 ( + )
    )
    |> ignore

end

module Sequence_impl = struct
  let int_sum () =
    Sequence.of_list int_list
    |> Sequence.fold ~init:0 ~f:( + )
    |> ignore

  let int_sumOfSquares () =
    Sequence.of_list int_list
    |> Sequence.map ~f:square
    |> Sequence.fold ~init:0 ~f:( + )
    |> ignore
end

module Stream_impl = struct
  open CFStream

  let int_sum () =
    Stream.of_list int_list
    |> Stream.fold ~init:0 ~f:( + )
    |> ignore

  let int_sumOfSquares () =
    Stream.of_list int_list
    |> Stream.map ~f:square
    |> Stream.fold ~init:0 ~f:( + )
    |> ignore
end


let command =
  Command.group ~summary:"Strymonas benchmark" [
    "sum", Bench.make_command [
      Bench.Test.create ~name:"sum::csequence" Cruanes_sequence_impl.int_sum ;
      Bench.Test.create ~name:"sum::cseq" Cseq_impl.int_sum ;
      Bench.Test.create ~name:"sum::enum" Enum_impl.int_sum ;
      Bench.Test.create ~name:"sum::gen" Gen_impl.int_sum ;
      Bench.Test.create ~name:"sum::imp" Imp_impl.int_sum ;
      Bench.Test.create ~name:"sum::pipes" Pipes_impl.int_sum ;
      Bench.Test.create ~name:"sum::sequence" Sequence_impl.int_sum ;
      Bench.Test.create ~name:"sum::stream" Stream_impl.int_sum ;
    ] ;
    "sumOfSquares", Bench.make_command [
      Bench.Test.create ~name:"sumOfSquares::csequence" Cruanes_sequence_impl.int_sumOfSquares ;
      Bench.Test.create ~name:"sumOfSquares::cseq" Cseq_impl.int_sumOfSquares ;
      Bench.Test.create ~name:"sumOfSquares::enum" Enum_impl.int_sumOfSquares ;
      Bench.Test.create ~name:"sumOfSquares::gen" Gen_impl.int_sumOfSquares ;
      Bench.Test.create ~name:"sumOfSquares::imp" Imp_impl.int_sumOfSquares ;
      Bench.Test.create ~name:"sumOfSquares::pipes" Pipes_impl.int_sumOfSquares ;
      Bench.Test.create ~name:"sumOfSquares::sequence" Sequence_impl.int_sumOfSquares ;
      Bench.Test.create ~name:"sumOfSquares::stream" Stream_impl.int_sumOfSquares ;
    ] ;
  ]

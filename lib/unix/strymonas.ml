module Cruanes_sequence = Sequence
open Core.Std
open Core_bench.Std

let int_list = List.range 0 1_000_000
let square = (fun x -> x * x)
let even n = n mod 2 = 0

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

  let int_sumOfSquaresEven () =
    S.of_list int_list
    |> S.filter even
    |> S.map square
    |> S.fold ( + ) 0
    |> ignore
end

module Cseq_impl = struct
  module S = Cseq

  let int_sum () =
    S.of_list int_list
    |> S.fold_left ( + ) 0
    |> ignore

  let int_sumOfSquares () =
    S.of_list int_list
    |> S.map square
    |> S.fold_left ( + ) 0
    |> ignore

  let int_sumOfSquaresEven () =
    S.of_list int_list
    |> S.filter even
    |> S.map square
    |> S.fold_left ( + ) 0
    |> ignore
end

module Enum_impl = struct
  open Batteries
  module S = Enum

  let int_sum () =
    List.enum int_list
    |> S.fold ( + ) 0
    |> ignore

  let int_sumOfSquares () =
    List.enum int_list
    |> S.map square
    |> S.fold ( + ) 0
    |> ignore

  let int_sumOfSquaresEven () =
    List.enum int_list
    |> S.filter even
    |> S.map square
    |> S.fold ( + ) 0
    |> ignore
end

module Gen_impl = struct
  module S = Gen

  let int_sum () =
    S.of_list int_list
    |> S.fold ( + ) 0

  let int_sumOfSquares () =
    S.of_list int_list
    |> S.map square
    |> S.fold ( + ) 0

  let int_sumOfSquaresEven () =
    S.of_list int_list
    |> S.filter even
    |> S.map square
    |> S.fold ( + ) 0
    |> ignore

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

  let int_sumOfSquaresEven () =
    let l = ref int_list in
    let sum = ref 0 in
    while !l <> [] do
      let n = List.hd_exn !l in
      if even n then (sum := !sum + n * n) ;
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

  let int_sumOfSquaresEven () =
    run (
      from_list int_list
      $$ filter even
      $$ map square
      $$ fold 0 ( + )
    )
    |> ignore

end

module Sequence_impl = struct
  module S = Sequence

  let int_sum () =
    S.of_list int_list
    |> S.fold ~init:0 ~f:( + )
    |> ignore

  let int_sumOfSquares () =
    S.of_list int_list
    |> S.map ~f:square
    |> S.fold ~init:0 ~f:( + )
    |> ignore

  let int_sumOfSquaresEven () =
    S.of_list int_list
    |> S.filter ~f:even
    |> S.map ~f:square
    |> S.fold ~f:( + ) ~init:0
    |> ignore

end

module Stream_impl = struct
  module S = CFStream.Stream

  let int_sum () =
    S.of_list int_list
    |> S.fold ~init:0 ~f:( + )
    |> ignore

  let int_sumOfSquares () =
    S.of_list int_list
    |> S.map ~f:square
    |> S.fold ~init:0 ~f:( + )
    |> ignore

  let int_sumOfSquaresEven () =
    S.of_list int_list
    |> S.filter ~f:even
    |> S.map ~f:square
    |> S.fold ~f:( + ) ~init:0
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
    "sumOfSquaresEven", Bench.make_command [
      Bench.Test.create ~name:"sumOfSquaresEven::csequence" Cruanes_sequence_impl.int_sumOfSquaresEven ;
      Bench.Test.create ~name:"sumOfSquaresEven::cseq" Cseq_impl.int_sumOfSquaresEven ;
      Bench.Test.create ~name:"sumOfSquaresEven::enum" Enum_impl.int_sumOfSquaresEven ;
      Bench.Test.create ~name:"sumOfSquaresEven::gen" Gen_impl.int_sumOfSquaresEven ;
      Bench.Test.create ~name:"sumOfSquaresEven::imp" Imp_impl.int_sumOfSquaresEven ;
      Bench.Test.create ~name:"sumOfSquaresEven::pipes" Pipes_impl.int_sumOfSquaresEven ;
      Bench.Test.create ~name:"sumOfSquaresEven::sequence" Sequence_impl.int_sumOfSquaresEven ;
      Bench.Test.create ~name:"sumOfSquaresEven::stream" Stream_impl.int_sumOfSquaresEven ;
    ] ;
  ]

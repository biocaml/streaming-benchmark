(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Simon Cruanes                                          *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Seq]: functional iterators *)

type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

let empty () = Nil

let return x () = Cons (x, empty)

let rec map f seq () = match seq() with
  | Nil -> Nil
  | Cons (x, next) -> Cons (f x, map f next)

let rec filter_map f seq () = match seq() with
  | Nil -> Nil
  | Cons (x, next) ->
      match f x with
        | None -> filter_map f next ()
        | Some y -> Cons (y, filter_map f next)

let rec filter f seq () = match seq() with
  | Nil -> Nil
  | Cons (x, next) ->
      if f x
      then Cons (x, filter f next)
      else filter f next ()

let rec flat_map f seq () = match seq () with
  | Nil -> Nil
  | Cons (x, next) ->
    flat_map_app f (f x) next ()

(* this is [append seq (flat_map f tail)] *)
and flat_map_app f seq tail () = match seq () with
  | Nil -> flat_map f tail ()
  | Cons (x, next) ->
    Cons (x, flat_map_app f next tail)

let fold_left f acc seq =
  let rec aux f acc seq = match seq () with
    | Nil -> acc
    | Cons (x, next) ->
        let acc = f acc x in
        aux f acc next
  in
  aux f acc seq

let iter f seq =
  let rec aux seq = match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f x;
        aux next
  in
  aux seq

type 'a memo_cell =
  | Memo_lazy of 'a t (* not evaluated yet *)
  | Memo_nil
  | Memo_cons of 'a * 'a memo_cell ref

(* memoize seq on the fly *)
let memo seq =
  let rec memoized (cell: 'a memo_cell ref) (): 'a node =
    match !cell with
      | Memo_cons (x, tail) -> Cons (x, memoized tail)
      | Memo_nil -> Nil
      | Memo_lazy seq ->
          match seq() with
            | Nil ->
                cell := Memo_nil; Nil
            | Cons (x, seq_tail) ->
                let tail = ref (Memo_lazy seq_tail) in
                cell := Memo_cons (x, tail);
                Cons (x, memoized tail)
  in
  let cell = ref (Memo_lazy seq) in
  memoized cell

let of_list l =
  let rec aux l () = match l with
    | [] -> Nil
    | x :: tail -> Cons (x, aux tail)
  in
  aux l

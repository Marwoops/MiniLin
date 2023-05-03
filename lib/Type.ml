open Sexplib.Std
open Ppx_compare_lib.Builtin

module V = Symbol.Make()

type id = V.t [@@deriving sexp]

let compare_id = V.compare
let equal_id = V.equal

type t =
  | Named of id
  | LinMap of t * t
  | Tensor of t list
                [@@deriving sexp, compare, equal]

(* Operator priority: t -o u * v = t -o (u * v), i.e., the linear arrow has
   lower precedence than the tensor product. *)

let precedence_of_head_type_constructor = function
  | Named _ -> max_int (* irrelevant *)
  | LinMap _ -> 10
  | Tensor _ -> 20

let rec pp ff =
  let open Fmt in
  function
  | (Named _ | Tensor _) as a ->
     pp_simple ff a
  | LinMap (t, u) ->
     Format.fprintf ff "@[%a -o@ %a@]"
       pp_simple t
       pp u

and pp_simple ff =
  let open Fmt in
  function
  | Named a ->
     V.pp ff a
  | Tensor ts ->
     (parens @@ list ~sep:(const string "*") pp) ff ts
  | t ->
     parens pp ff t

open Sexplib.Std

type t = float [@@deriving sexp]

let pp = Fmt.float

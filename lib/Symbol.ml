(* This is a non-optimized implementation, as strings are not interned. *)

open Sexplib.Std

module type S = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val of_string : string -> t
  val to_string : t -> string

  val pp : t Fmt.t

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
end

module Make() = struct
  module M = struct
    type t = string [@@deriving sexp]

    let equal (x : string) y = x = y

    let compare (x : string) = Stdlib.compare x
  end

  include M

  let of_string x = x

  let to_string x = x

  let pp fmt x = Format.fprintf fmt "%s" x

  module Map = Map.Make(M)
  module Set = Set.Make(M)
end

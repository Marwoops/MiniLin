(** {1 Symbols} *)

(** Symbols are values that support efficient equality and comparison checks, as
    well as conversion to and from strings and S-expressions. *)
module type S = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val of_string : string -> t
  val to_string : t -> string

  val pp : t Fmt.t

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  (** This module type also includes symbol sets and symbol-indexed maps for the
      user's convenience. They have been defined using the [Stdlib.Set] and
      [Stdlib.Map] modules, and share their performance characteristics. *)

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
end

(** Create a new symbol module, giving rise to a symbol type distinct from all
    the other ones. *)
module Make() : S

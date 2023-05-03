(** {1 {i MiniLin} Types} *)

(** {2 Identifiers} *)

module V : Symbol.S

type id = V.t

(** {2 Types} *)

(** The OCaml type of {i MiniLin} types. *)
type t =
  | Named of id
  (** Name of a type declared through a previous [type] declaration. *)
  | LinMap of t * t
  (** Linear maps. *)
  | Tensor of t list
  (** N-ary tensor product. The unit type is the special case [Tensor []]. *)

(** {2 Pretty-printing} *)

val pp : t Fmt.t

(** {2 Utility functions} *)

val id_of_sexp : Sexplib.Sexp.t -> id
val sexp_of_id : id -> Sexplib.Sexp.t
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t
val compare : t -> t -> int
val equal : t -> t -> bool

(** {1 Type-checking for {i MiniLin} } *)

(** {2 Infrastructure code} *)

type id = Surface.id

(** {2 Errors common to both typechecking modes} *)

module Error : sig
  type t =
    | Unbound_identifier of id
    | Unbound_constructor of string
    | Nonlinear_use of id * [`Contraction | `Weakening]
    | Type_mismatch of { expected : [ `C of Type.t | `LinMap | `Tensor ];
                         actual : Type.t; }
    | Could_not_infer of [`Term of Surface.term | `Pattern of Surface.pattern]
    | Unimplemented

  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val reason : t -> [`Unbound_identifier
                    | `Unbound_constructor
                    | `Nonlinear_use
                    | `Type_mismatch
                    | `Could_not_infer
                    | `Unimplemented ]
end

module Ctx : sig
  (** The abstract (OCaml) type of typing contexts. *)
  type v

  (** The empty context. *)
  val empty : v

  (** [bind_var g x t] adds the binding [x : t] to the context [g]. *)
  val bind_var : v -> id -> Type.t -> v

  (** [bind_enum g a t] adds the binding [a : t] to the context [g]. *)
  val bind_enum : v -> Type.id -> (Surface.constr * Type.t list) list -> v

  (** [bind_alias g a t] adds the binding [a : t] to the context [g]. *)
  val bind_alias : v -> Type.id -> Type.t -> v

  (** [free_var g x] returns [Right n g'] when [g'] is the context where the last
   variable named [x] has been removed and [n] its uses count.*)
  val free_var : v -> id -> (Error.t, IntPlusInf.t * v) Either.t

  (** [union g d] returns the multiset-theoretic union of [g] and [d]. *)
  val union : v -> v -> v

  (** [difference g d] returns [Right t] when [t] is the multiset-theoretic
      difference [g] minus [d]. This operation is only defined when [g] is
      larger than [d], otherwise [difference g d] returns [Left x] where [x] is
      a variable that has more uses in [d] than in [g]. In particular, if [x]
      does not appear in [g], we consider that it has [0] uses in it.

      This function only deals with use counts. If [x] appears in both [g] and
      [d] with distinct types, the result is undefined. *)
  (* val difference : v -> v -> (Error.t, v) Either.t *)

  (** [lookup g x] returns [Right (t, n) g'] when [t] is the type associated with the last variable named [x] in [g]
      as well as [n] its number of uses, together with a copy of [g] where the number of uses of
      [x] has been incremented. If [x] is not present in [g], this function returns [Left Error.Unbound_identifier x].
    *)
  val lookup : v -> id -> (Error.t, (Type.t * IntPlusInf.t) * v) Either.t

  (** [lookup_constr g k] returns [Right (ty, ts)] where [ty] is the type to which
      constructor [k] in [g] belongs and [ts] gives the types of the parameters
      of [k]. If [k] is not present in [g], this function raises returns [Left Error.Unbound_constructor k]. *)
  val lookup_constr : v -> Surface.constr ->
      (Error.t, Type.t * Type.t list) Either.t
end

(** {2 Forward-mode type checker} *)

(** The following module check types in "forward" mode, that is, receives an
    input environment and produces the output type (if any). *)

module ForwardDirect : sig
  val infer_term : Surface.term -> Ctx.v -> (Error.t, Type.t * Ctx.v) Either.t

  val file : Surface.t -> Ctx.v -> (Error.t, Ctx.v) Either.t
end

module Forward : sig
  include Structure.Monad

  val error : Error.t -> 'a t

  val run : 'a t -> Ctx.v -> (Error.t, 'a) Either.t

  (** [term m] returns the monadic value associated with the term [m]. Running
      this value gives you the result of typechecking for [m]. *)
  val infer_term : Surface.term -> 'a t

  (** [file f] returns the monadic value associated with the file [f]. Running
      this value gives you the result of typechecking for [f]. *)
  val file : Surface.t -> 'a t
end

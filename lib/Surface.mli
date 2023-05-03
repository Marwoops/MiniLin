(** {1 {i MiniLin}'s Surface Syntax Tree} *)

(** This is the syntax tree for {i MiniLin}'s surface language, as entered
    directly by the programmer. It strives to be reasonably convenient to
    program in, at the expanse of minimality: some constructs are special case
    of others. *)

(** {2 Identifiers} *)

module V : Symbol.S

type id = V.t

(** {2 Abstract syntax tree} *)

(** The type of constructor name. Always begin with a capital letter in concrete
    syntax, but at this level we do not care. *)
type constr = string

(** The type of terms, that is, expressions. This is the most important type in
    this module, since it defines the constructions available in {i MiniLin}. *)
type term =
  | Var of id
  (** Variable occurence. *)
  | Lam of binding
  (** Lambda abstraction. *)
  | App of term * term
  (** Application. *)
  | Tuple of term list
  (** Element of some n-ary tensor product. *)
  | Constr of constr * term list
  (** Application of a constructor. *)
  | Match of term * binding list
  (** Local let binding. *)
  | Let of term * binding
  (** Pattern matching. *)
  | Weight of Real.t * term
  (** Multiplication of a program by a real number. *)
  | Sum of term * term
  (** Nondeterministic sum of two programs. *)
  | Annot of term * Type.t
  (** Type annotation *)

(** A binding is given by a pattern and a term, the pattern's variables being
    bound in the term. *)
and binding = pattern * term

(** A pattern describes and binds value shapes. *)
and pattern =
  | Pvar of id
  (** Variable. *)
  | Ptuple of pattern list
  (** N-ary tuple. *)
  | Pconstr of constr * pattern list
  (** Constructor of enumerated type. *)
  | Pannot of pattern * Type.t
  (** Type annotation. *)

(** A definition gives a name to a term or to a type. As in OCaml, this includes
    sum type declarations introducing new constructors. *)
type definition =
  | Val of id * Type.t option * term
  (** A value binding, with an optional type annotation. *)
  | Fun of id * pattern list * Type.t * term
  (** A function, with the return type annotated. *)
  | Enum of Type.id * (constr * Type.t list) list
  (** A sum (enumerated) type declaration. Such declarations are {b not}
      recursive, unlike OCaml's: the list of parameters of a constructor cannot
      mention the type being declared. *)
  | Alias of Type.id * Type.t
  (** A simple type alias definition. *)

(** A program is a list of definitions. *)
type t = definition list

(** {2 Pretty-printing} *)

val pp_term : term Fmt.t
val pp_binding : binding Fmt.t
val pp_pattern : pattern Fmt.t
val pp_definition : definition Fmt.t
val pp : t Fmt.t

(** {2 Conversion functions} *)

val id_of_sexp : Sexplib.Sexp.t -> id
val sexp_of_id : id -> Sexplib.Sexp.t
val constr_of_sexp : Sexplib.Sexp.t -> constr
val sexp_of_constr : constr -> Sexplib.Sexp.t
val term_of_sexp : Sexplib.Sexp.t -> term
val sexp_of_term : term -> Sexplib.Sexp.t
val definition_of_sexp : Sexplib.Sexp.t -> definition
val sexp_of_definition : definition -> Sexplib.Sexp.t
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

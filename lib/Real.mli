(** {1 Real Numbers} *)

(** We sacrifice mathematical exactness for efficiency here: as far as we're
    concerned, real numbers are simply floating-point numbers. Time will tell
    whether that is a problem in practice.  *)

type t = float

val sexp_of_t : t -> Sexplib.Sexp.t
val t_of_sexp : Sexplib.Sexp.t -> t

val pp : t Fmt.t

type t = int * int

val pp : t Fmt.t

val of_lexing_pos : Lexing.position -> t

type range = t * t

val pp_range : range Fmt.t

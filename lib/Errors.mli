type t

exception Error of t

val pp : t Fmt.t

val unknown_character : Position.t -> string -> 'a
val unterminated_comment : Position.t -> 'a
val syntax : Position.range -> 'a

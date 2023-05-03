type t =
  | Unknown_character of Position.t * string
  | Unterminated_comment of Position.t
  | Syntax of Position.range

exception Error of t

let pp ff = function
  | Unknown_character (p, c) ->
     Format.fprintf ff "%a: unknown character ('%s')"
       Position.pp p
       c
  | Unterminated_comment p ->
     Format.fprintf ff "%a: unterminated comment"
       Position.pp p
  | Syntax r ->
     Format.fprintf ff "%a: syntax error"
       Position.pp_range r

let error cause = raise (Error cause)

let unknown_character p c = error (Unknown_character (p, c))
let unterminated_comment p = error (Unterminated_comment p)
let syntax r = error (Syntax r)


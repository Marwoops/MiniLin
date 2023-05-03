type t = int * int

let pp ff (l, c) = Format.fprintf ff "%d:%d" l c

let of_lexing_pos lexpos =
  Lexing.(lexpos.pos_lnum, lexpos.pos_cnum - lexpos.pos_bol)

type range = t * t

let pp_range ff ((l_beg, c_beg), (l_end, c_end)) =
  if l_beg = l_end then Format.fprintf ff "%d:%d-%d" l_beg c_beg c_end
  else Format.fprintf ff "%d:%d-%d:%d" l_beg c_beg l_end c_end

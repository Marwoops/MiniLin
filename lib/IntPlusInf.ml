type t = NInf | PInf | Int of int

let equal x x' = match x, x' with
  | NInf, NInf | PInf, PInf -> true
  | Int k, Int k' -> k = k'
  | _ -> false

let compare x x' = match x, x' with
  | PInf, PInf | NInf, NInf -> 0
  | PInf, _ | _, NInf -> 1
  | NInf, _ | _, PInf -> -1
  | Int k, Int k' -> k - k'

let ( + ) x x' = match x, x' with
  | Int k, Int k' -> Int (k + k')
  | PInf, NInf | NInf, PInf -> Int 0
  | PInf, _ | _, PInf -> PInf
  | NInf, _ | _ ,NInf -> NInf

let ( - ) x x' = match x, x' with
  | Int k, Int k' -> Int (k - k')
  | PInf, PInf | NInf, NInf-> Int 0
  | PInf, NInf | PInf, Int _ | Int _, NInf -> PInf
  | NInf, PInf | Int _, PInf | NInf, Int _ -> NInf

let ( = ) = Stdlib.( = )
let ( < ) x x' = compare x x' < 0

let of_int k = Int k

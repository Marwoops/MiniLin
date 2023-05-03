open Sexplib.Std

module V = Symbol.Make()

type id = V.t [@@deriving sexp]

type constr = string [@@deriving sexp]

type term =
  | Var of id
  | Lam of binding
  | App of term * term
  | Tuple of term list
  | Constr of constr * term list
  | Match of term * binding list
  | Let of term * binding
  | Weight of Real.t * term
  | Sum of term * term
  | Annot of term * Type.t
                      [@@deriving sexp]

and binding = pattern * term

and pattern =
  | Pvar of id
  | Ptuple of pattern list
  | Pconstr of constr * pattern list
  | Pannot of pattern * Type.t

type definition =
  | Val of id * Type.t option * term
  | Fun of id * pattern list * Type.t * term
  | Enum of Type.id * (constr * Type.t list) list
  | Alias of Type.id * Type.t
                  [@@deriving sexp]

type t = definition list [@@deriving sexp]

let rec pp_term ff =
  let open Fmt in
  function
  | (Var _ | Weight _) as m ->
     pp_simple_term ff m
  | Lam b ->
     Format.fprintf ff "@[fun %a@]"
       pp_binding b
  | App (m, n) ->
     Format.fprintf ff "@[%a@ %a@]"
       pp_term m
       pp_simple_term n
  | Tuple ms ->
     tuple pp_term ff ms
  | Constr (k, ms) ->
     constr_app pp_term ff (k, ms)
  | Match (m, clauses) ->
     Format.fprintf ff "@[match %a@ %a@]"
       pp_term m
       (braces @@ list ~sep:(const string "|") pp_binding) clauses
  | Let (m, (p, n)) ->
     Format.fprintf ff "@[let @[%a =@ %a@] in@ %a@]"
       pp_pattern p
       pp_term m
       pp_term n
  | Sum (m, n) ->
     Format.fprintf ff "@[%a +@ %a@]"
       pp_term m
       pp_term n
  | Annot (m, t) ->
     Format.fprintf ff "(@[%a :@ %a@])"
       pp_term m
       Type.pp t

and angle : type a. a Fmt.t -> a Fmt.t =
  fun pp ff x -> Format.fprintf ff "@[<1><%a>@]" pp x

and constr_app : type a. a Fmt.t -> (string * a list) Fmt.t =
  fun pp ->
  Fmt.(box @@ pair string (angle (list ~sep:(const string ",") pp)))

and tuple : type a. a Fmt.t -> a list Fmt.t =
  fun pp -> Fmt.(parens @@ list ~sep:(const string ",") pp)

and pp_simple_term ff =
  let open Fmt in
  function
  | Var x ->
     V.pp ff x
  | Weight (r, m) ->
     Format.fprintf ff "%a.%a"
       Real.pp r
       pp_simple_term m
  | m ->
     parens pp_term ff m

and pp_pattern ff =
  let open Fmt in
  function
  | Pvar x ->
     V.pp ff x
  | Ptuple ps ->
     tuple pp_pattern ff ps
  | Pconstr (k, ps) ->
     constr_app pp_pattern ff (k, ps)
  | Pannot (p, ty) ->
     Format.fprintf ff "%a : %a"
       pp_pattern p
       Type.pp ty

and pp_binding ff (p, m) =
  Format.fprintf ff "@[<hv 2>%a =o@ %a@]"
    pp_pattern p
    pp_term m

let pp_definition ff =
  let open Fmt in
  function
  | Val (x, tyo, m) ->
     Format.fprintf ff "@[<hv 2>val %a%a@ = %a@]"
       V.pp x
       (option (const string ": " ++ Type.pp)) tyo
       pp_term m
  | Fun (x, ps, ty, m) ->
     Format.fprintf ff "@[<hv 2>fun %a@ %a@ : %a@ =@ %a"
       V.pp x
       (list pp_pattern) ps
       Type.pp ty
       pp_term m
  | Enum (a, constrs) ->
     Format.fprintf ff "@[<hv 2>type %a =@ %a@]"
       Type.V.pp a
       (list ~sep:(const string "|") (constr_app Type.pp)) constrs
  | Alias (a, t) ->
     Format.fprintf ff "@[<hv 2>type %a =@ %a@]"
       Type.V.pp a
       Type.pp t

let pp = Fmt.list ~sep:(fun ff () -> Format.fprintf ff "@\n") pp_definition

open Ppx_compare_lib.Builtin

type id = Surface.id
let compare_id = Surface.V.compare
let equal_id = Surface.V.equal

module Error = struct
  type t =
    | Unbound_identifier of id
    | Unbound_constructor of string
    | Nonlinear_use of id * [`Contraction | `Weakening]
    | Type_mismatch of { expected : [ `C of Type.t | `LinMap | `Tensor ];
                         actual : Type.t; }
    | Could_not_infer of ([ `Term of Surface.term
                          | `Pattern of Surface.pattern ] [@ignore])
    | Unimplemented [@@deriving compare, equal]

  let pp fmt = function
    | Unbound_identifier x ->
       Format.fprintf fmt "unbound identifier %a"
         Surface.V.pp x
    | Unbound_constructor k ->
       Format.fprintf fmt "unbound constructor %s"
         k
    | Nonlinear_use (x, `Contraction) ->
       Format.fprintf fmt "identifier %a has been used more than once"
         Surface.V.pp x
    | Nonlinear_use (x, `Weakening) ->
       Format.fprintf fmt "identifier %a has not been used"
         Surface.V.pp x
    | Type_mismatch { expected; actual; } ->
       let pp ff = function
         | `C t -> Type.pp ff t
         | `LinMap -> Format.fprintf ff "_ -> _"
         | `Tensor -> Format.fprintf ff "_ * ... * _"
       in
       Format.fprintf fmt "type %a is incompatible with expected type %a"
         Type.pp actual
         pp expected
    | Could_not_infer a ->
       let pp ff = function
         | `Term tm -> Surface.pp_term ff tm
         | `Pattern ty -> Surface.pp_pattern ff ty
       in
       Format.fprintf fmt "@[could not infer type of@ @[%a@]@]"
         pp a
    | Unimplemented ->
       Format.fprintf fmt "feature not yet implemented"

  let reason = function
    | Unbound_identifier _ -> `Unbound_identifier
    | Unbound_constructor _ -> `Unbound_constructor
    | Nonlinear_use _ -> `Nonlinear_use
    | Type_mismatch _ -> `Type_mismatch
    | Could_not_infer _ -> `Could_not_infer
    | Unimplemented -> `Unimplemented
end

module Id = struct
    type t = (Surface.id, Type.id) Either.t
    let compare id id' =
        match id, id' with
        | Either.Left x, Either.Left x' -> Surface.V.compare x x'
        | Either.Right x, Either.Right x' -> Type.compare (Type.Named x) (Type.Named x')
        | Either.Left _, Either.Right _ -> 1
        | Either.Right _, Either.Left _ -> -1
end

module Ctx = struct
  include Map.Make (Id)

  type var_class = { t : Type.t; uses : IntPlusInf.t; }

  type entry =
    | Var of var_class list     (* INVARIANT: never empty *)
    | Alias of { t : Type.t; }
    | Enum of { constrs : (Surface.constr * Type.t list) list; }

  type v = entry t

  let bind_enum g a constrs = add (Either.Right a) (Enum { constrs; }) g

  let bind_alias g a t = add (Either.Right a) (Alias { t; }) g

  let bind_var g x t =
    update (Either.Left x)
    (function | Some (Var xentries) ->
                Some (Var ({ t; uses = IntPlusInf.of_int 0; } :: xentries))
              | Some ( Alias _ | Enum _) ->
                None
              | None ->
                Some (Var [{ t; uses = IntPlusInf.of_int 0; }]))
    g

  let update default x f g =
    let r = ref (Either.Left default) in
    let g = update
              (Either.Left x)
              (fun entry ->
                match f entry with
                | None, new_entry -> new_entry
                | Some v, new_entry -> r := Either.Right v; new_entry)
              g
    in
    Either.map_right (fun v -> (v, g)) !r

  let free_var g x =
    update
      (Error.Unbound_identifier x)
      x
      (function Some (Var [{ uses; _; }]) ->
                 Some uses, None
              | Some (Var ({ uses; _; } :: xentries)) ->
                 Some uses, Some (Var xentries)
              | Some (Var [] | Alias _ | Enum _) | None ->
                 None, None)
      g

  let lookup g x : (Error.t, (Type.t * IntPlusInf.t) * v) Either.t =
    update
      (Error.Unbound_identifier x)
      x
      (function Some (Var ({ uses; t; } :: xentries)) ->
                 let uses = IntPlusInf.(uses + of_int 1) in
                 Some (t, uses),
                 Some (Var ({ uses; t; } :: xentries))
              | Some (Var [] | Alias _ | Enum _) | None ->
                 None, None)
      g

  let lookup_constr g k =
    match
        find_first_opt
        (fun key -> match find key g with
        | Var _ | Alias _ -> false
        | Enum { constrs; } -> List.exists (fun (k', _) -> k = k') constrs)
        g
    with
    | Some (Either.Right a, Enum { constrs; }) ->
        let ts = List.assoc k constrs in
        Either.Right (Type.Named a, ts)
    | Some _ | None -> Either.Left (Error.Unbound_constructor k)

  let contains_all f xs =
    List.for_all (fun x -> List.exists (f x) xs)

  let equal_list f  xs ys =
    contains_all f xs ys && contains_all f ys xs

  let var_class_equal v v' =
    match v, v' with
    | { t; uses; }, { t = t'; uses = uses' } ->
        Type.equal t t' && IntPlusInf.equal uses uses'

  let entry_equal k k' =
    match k, k' with
    | Var xentries, Var xentries' -> 
        begin
        try
            List.fold_right2
            (fun v v' acc -> var_class_equal v v' && acc)
            xentries xentries' true
        with Invalid_argument _ -> false
        end
    | Alias { t; }, Alias { t = t'; } ->
        Type.equal t t'
    | Enum { constrs; }, Enum { constrs = constrs'; } ->
        equal_list
        (fun (c, t) (c', t') -> equal_list Type.equal t t' && c = c')
        constrs constrs'
    | _ -> false

  let equal = equal entry_equal

  let combine f =
    union
      (fun _ k k' ->
        match k, k' with
        | Var xentries, Var xentries' ->
            Some (Var (f xentries xentries'))
        | Enum { constrs; }, Enum _ ->
           Some (Enum { constrs; })
        | Alias { t; }, Alias _ ->
           Some (Alias { t; })
        | _ -> None)

  let union = combine ( @ )
  (* exception Overconsumed of id *)

  (* let difference g d = *)
  (*   try *)
  (*     Either.Right *)
  (*       (combine (fun x t a b -> *)
  (*            let open IntPlusInf in *)
  (*            if a = b then None *)
  (*            else if a > b then Some (Var { t; uses = IntPlusInf.(a - b); }) *)
  (*            else raise (Overconsumed x)) g d) *)
  (*   with Overconsumed x -> *)
  (*     Either.Left (Error.Nonlinear_use (x, `Contraction)) *)
end

module ForwardDirect = struct
  module M = struct
    type 'a t = (Error.t, 'a) Either.t

    let return x = Either.Right x

    let ( let* ) x f = match x with Either.Left err -> Either.Left err
                                  | Either.Right x -> f x
  end

  module MUtils = MonadUtils.Make(M)
  open MUtils
  open M

  (* [check_pattern pa ty g] succeeds with [Either.Right g'] when the
     pattern [pa] matches values of type [ty], adding new bindings to the
     input environment [g] which results in a new environment [g']. *)
  let rec check_pattern
            (pa : Surface.pattern) (expected : Type.t) (g : Ctx.v)
          : (Error.t, Ctx.v) Either.t =
    match pa with
    | Surface.Pvar id ->
       Either.Right (Ctx.bind_var g id expected)

    | Surface.Ptuple ps ->
       begin match expected with
       | Tensor expecteds ->
          (* TODO: improve error message. Right now the exception raised by
             fold_right2_mon leaks to the user. *)
          let* g = fold_right2_mon check_pattern ps expecteds g in
          return g
       | actual ->
          Either.Left (Error.Type_mismatch {expected = `Tensor; actual; })
       end

    | Surface.Pannot _ | Surface.Pconstr _ ->
       let* actual, g' = infer_pattern pa g in
       if Type.equal expected actual
       then Either.Right g'
       else Either.Left (Error.Type_mismatch { expected = `C expected;
                                               actual; })

  (* [infer_pattern pa g] succeeds with [Either.Right (ty, g')] when the pattern
     [pa] matches values of type [ty], adding new bindings to the input
     environment [g] which results in a new environment [g']. *)
  and infer_pattern (pa : Surface.pattern) (g : Ctx.v)
      : (Error.t, Type.t * Ctx.v) Either.t =
    match pa with
    | Surface.Pvar _ | Surface.Ptuple _ ->
        Either.Left (Error.Could_not_infer (`Pattern pa))

    | Pannot (p, ty) ->
        let* g' = check_pattern p ty Ctx.empty in
        return (ty, Ctx.union g' g)

    | Pconstr (k, ps) ->
        let* ty, expecteds = Ctx.lookup_constr g k in
        let* g = fold_right2_mon check_pattern ps expecteds g in
        Either.Right (ty, g)

  (* [consume_pattern pa g] succeeds with [Either.Right g'] when all the
     variables in the pattern [pa] are present with a use count of one in
     [g], with [g'] the least environment with those variables removed. *)
  let rec consume_pattern (pa : Surface.pattern) (g : Ctx.v)
          : (Error.t, Ctx.v) Either.t =
    match pa with
    | Surface.Pvar x ->
       let* uses, g = Ctx.free_var g x in
       if IntPlusInf.(uses < of_int 1)
       then Either.Left (Error.Nonlinear_use (x, `Weakening))
       else if IntPlusInf.(uses > of_int 1) then Either.Left (Error.Nonlinear_use (x, `Contraction))
       else return g
    | Surface.Ptuple ps | Surface.Pconstr (_, ps) ->
       fold_right_mon consume_pattern ps g
    | Surface.Pannot (p, _) ->
       consume_pattern p g

  let rec infer_term m g =
    match m with
    | Surface.Var x ->
       let* (ty, uses), g = Ctx.lookup g x in
       if IntPlusInf.(uses > of_int 1)
       then Either.Left (Error.Nonlinear_use (x, `Contraction))
       else return (ty, g)
    | Surface.App (m, n) ->
        let* ty_m, g = infer_term m g in
        begin match ty_m with
        | Type.LinMap (ty_a, ty_b) ->
           let* g = check_term n ty_a g in
           return (ty_b, g)
        | actual ->
           Either.Left (Error.Type_mismatch { expected = `LinMap; actual; })
        end
    | Surface.Lam b ->
       let* ty_dom, ty_cod, g = infer_binding g b in
       return (Type.LinMap (ty_dom, ty_cod), g)
    | Surface.Tuple ms ->
       let* tys, g =
         fold_right_mon
           (fun m (tys, g) -> let* ty, g = infer_term m g in return (ty :: tys, g))
           ms
           ([], g)
       in
       return (Type.Tensor tys, g)
    | Surface.Let (e, b) ->
       let* ty_e, g = infer_term e g in
       check_binding g ty_e b
    | Surface.Weight (_, m) ->
       infer_term m g
    | Surface.Sum (m, n) ->
       let* ty, g = infer_term m g in
       let* g = check_term n ty g in
       return (ty, g)
    | Surface.Annot (m, expected) ->
       let* g = check_term m expected g in
       return (expected, g)
    | Surface.Constr (k, terms) ->
        let* ty, expecteds = Ctx.lookup_constr g k in
        let* g = fold_right2_mon check_term terms expecteds g in
        return (ty, g)

    | Surface.Match (m, bindings) as m' ->
        let* expected_dom, g = infer_term m g in
        match bindings with
        | [] ->
           Either.Left Error.(Could_not_infer (`Term m'))
        | (pa, m) :: bindings ->
            let* g' = check_pattern pa expected_dom g in
            let* expected_cod, g' = infer_term m g' in
            let* expected_g = consume_pattern pa g' in
            let* _ = for_all_mon
                (fun (pa, m) ->
                let* g' = check_pattern pa expected_dom g in
                let* g' = check_term m expected_cod g' in
                let* g' = consume_pattern pa g' in
                return (Ctx.equal g' expected_g))
                bindings
            in
            return (expected_cod, expected_g)

  and check_term m expected g =
    let* actual, g = infer_term m g in
    if Type.equal expected actual
    then return g
    else Either.Left (Error.Type_mismatch { expected = `C expected; actual;})

  and infer_binding g (p, m) =
    let* ty_dom, g = infer_pattern p g in
    let* ty_cod, g = infer_term m g in
    let* g = consume_pattern p g in
    return (ty_dom, ty_cod, g)

  and check_binding
      : Ctx.v -> Type.t -> Surface.binding -> (Type.t * Ctx.v) t =
    fun g ty_dom (p, m) ->
    let* g = check_pattern p ty_dom g in
    let* ty_cod, g = infer_term m g in
    let* g = consume_pattern p g in
    return (ty_cod, g)

  let rec file defs g =
    match defs with
    | [] -> return g
    | Surface.Val (x, annot, m) :: defs ->
       let m = match annot with None -> m | Some ty -> Surface.Annot (m, ty) in
       let* ty, g = infer_term m g in
       let g = Ctx.bind_var g x ty in
       file defs g

    | Surface.Fun (x, ps, expected_cod, m) :: defs ->
       (* let* ty_x, g_ps = *)
       (*   fold_right_mon *)
       (*     (fun pa (ty_cod, g_ps) -> *)
       (*       let* ty_dom, g_ps = infer_pattern pa g_ps in *)
       (*       return (Type.LinMap (ty_dom, ty_cod), g_ps)) *)
       (*     ps *)
       (*     (expected_cod, Ctx.empty) *)
       (* in *)
       (* let* g = check_term m expected_cod Ctx.(union g g_ps) in *)
       (* let* g = Ctx.difference g g_ps in *)
       (* let g = Ctx.bind_var g x ty_x in *)
       (* file defs g *)
       let rec aux g = function
        | [] ->
            let* g = check_term m expected_cod g in
            return (expected_cod, g)
        | pa :: ps ->
            let* dom, g = infer_pattern pa g in
            let* cod, g = aux g ps in
            let* g = consume_pattern pa g in
            return (Type.LinMap (dom, cod), g)
        in
        let* ty, g = aux g ps in
        let g = Ctx.bind_var g x ty in
        file defs g


    | Surface.Enum (a, constrs) :: defs ->
        let g = Ctx.bind_enum g a constrs in
        file defs g
    | Surface.Alias (a, ty) :: defs ->
        let g = Ctx.bind_alias g a ty in
        file defs g
end

module Forward = struct
  (* type 'a t = Ctx.v -> (Error.t, 'a * Ctx.v) Either.t *)
  type 'a t = Ctx.v -> (Error.t, 'a) Either.t

  (* let return x env = Either.Right (x, env) *)
  let return x _ = Either.Right x

  let ( let* ) : 'a t -> ('a -> 'b t) -> 'b t =
    fun k f env ->
    match k env with
    | Either.Left err -> Either.Left err
    | Either.Right x -> f x env

  let error : Error.t -> 'a t =
    fun err _ -> Either.Left err

  let run v env = v env

  let infer_term _ =
    (* A implémenter ! *)
    error Error.Unimplemented

  let file _ =
    (* Itou ! *)
    error Error.Unimplemented
end

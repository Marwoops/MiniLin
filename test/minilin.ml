open Minilin
module E = Typechecker.Error

let type_t = Alcotest.testable Type.pp Type.equal

let pp_tc_res ff = function
    | Either.Left err -> E.pp ff err
    | Either.Right t -> Type.pp ff t

let pp_tc_direct_res ff = function
    | Either.Left err -> E.pp ff err
    | Either.Right (t, _) -> Type.pp ff t

let tc_res_t =
  Alcotest.testable pp_tc_res
    (fun r1 r2 ->
      match r1, r2 with
      | Left err1, Left err2 -> E.reason err1 = E.reason err2
      | Right t1, Right t2 -> Type.equal t1 t2
      | _ -> false)

let tc_res_direct_t =
  Alcotest.testable pp_tc_direct_res
    (fun r1 r2 ->
      match r1, r2 with
      | Left err1, Left err2 -> E.reason err1 = E.reason err2
      | Right (t1, _), Right (t2, _) -> Type.equal t1 t2
      | _ -> false)

let test_typecheck_fwd_direct ?(ctx = Typechecker.Ctx.empty) ~id m res =
  let name =
    Format.asprintf "%s %a :? %a" id Surface.pp_term m pp_tc_direct_res res
  in
  let actual = Typechecker.ForwardDirect.infer_term m ctx in
  let test () = Alcotest.check tc_res_direct_t name res actual in
  Alcotest.test_case name `Quick test

let test_typecheck_fwd ?(ctx = Typechecker.Ctx.empty) ~id m res =
  let name =
    Format.asprintf "%s %a :? %a" id Surface.pp_term m pp_tc_res res
  in
  let test () =
    Alcotest.(check tc_res_t name res
                Typechecker.Forward.(run (infer_term m) ctx))
  in
  Alcotest.test_case name `Quick test

let test_typecheck_fwd_ok ?(ctx = Typechecker.Ctx.empty) ~id m t =
  test_typecheck_fwd ~ctx ~id m (Either.Right t)

let test_typecheck_fwd_ko ?(ctx = Typechecker.Ctx.empty) ~id m err =
  test_typecheck_fwd ~ctx ~id m (Either.Left err)

let test_typecheck_fwd_direct_ok ?(ctx = Typechecker.Ctx.empty) ~id m t =
  test_typecheck_fwd_direct ~ctx ~id m (Either.Right t)

let test_typecheck_fwd_direct_ko ?(ctx = Typechecker.Ctx.empty) ~id m err =
  test_typecheck_fwd_direct ~ctx ~id m (Either.Left err)

let unit_t = Type.Tensor []

let a = Type.V.of_string "a"
let b = Type.V.of_string "b"
let x = Surface.V.of_string "x"
let y = Surface.V.of_string "y"
let z = Surface.V.of_string "z"

let enum_env =
    let g =
    Typechecker.Ctx.bind_enum
    (Typechecker.Ctx.empty)
    a
    [("A", []); ("B", []);] in
    Typechecker.Ctx.bind_var g x (Type.Named a)

let fwd_tests =
  let u_to_u = Type.LinMap (unit_t, unit_t) in
  [
    test_typecheck_fwd_direct_ok ~id:"var"
      (Lam (Pannot (Pvar x, unit_t), Var x))
      (u_to_u, Typechecker.Ctx.empty);

    test_typecheck_fwd_direct_ko ~id:"var_contract"
      (Lam (Pannot (Pvar x, unit_t), Tuple [Var x; Var x]))
      E.(Nonlinear_use (x, `Contraction));

    test_typecheck_fwd_direct_ok ~id:"higher_order"
      (Lam (Pannot (Pvar x, LinMap(unit_t, unit_t)),
        App ((Lam (Pannot (Pvar y, u_to_u), Var y)), Var x)))
      (LinMap (u_to_u, u_to_u), Typechecker.Ctx.empty);

    test_typecheck_fwd_direct_ok ~id:"var_shadow"
      (Lam (Pannot (Pvar x, LinMap(unit_t, unit_t)),
        App ((Lam (Pannot (Pvar x, u_to_u), Var x)), Var x)))
      (LinMap (u_to_u, u_to_u), Typechecker.Ctx.empty);

    test_typecheck_fwd_direct_ok ~id:"var_shadow_alpha_renamed"
      (Lam (Pannot (Pvar x, LinMap(unit_t, unit_t)),
            App ((Lam (Pannot (Pvar y, u_to_u), Var y)), Var x)))
      (LinMap (u_to_u, u_to_u), Typechecker.Ctx.empty);

    test_typecheck_fwd_direct_ok ~id:"match_enum"
    ~ctx:enum_env
      (Match (Var x, [
          (Pconstr ("A", []), Constr("B", []));
          (Pconstr ("B", []), Constr("A", []));
      ]))
      (Type.Named a, Typechecker.Ctx.empty)
    ]

let () =
  Alcotest.run "types"
    [("fwd", fwd_tests)]

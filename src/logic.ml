open Ast

open Z3
(* open Z3.Expr *)

let ctx = Z3.mk_context []

let rec expr_to_z3 (e : expr) : Expr.expr =
  match e with
  | EInt i -> Z3.Arithmetic.Integer.mk_numeral_i ctx i
  | EBool b -> if b then Z3.Boolean.mk_true ctx else Z3.Boolean.mk_false ctx
  | EId id -> Z3.Expr.mk_const_s ctx id (Z3.Boolean.mk_sort ctx)
  | EAdd (e1, e2) -> Z3.Arithmetic.mk_add ctx [expr_to_z3 e1; expr_to_z3 e2]
  | ESub (e1, e2) -> Z3.Arithmetic.mk_sub ctx [expr_to_z3 e1; expr_to_z3 e2]
  | EMul (e1, e2) -> Z3.Arithmetic.mk_mul ctx [expr_to_z3 e1; expr_to_z3 e2]
  | EAnd (e1, e2) -> Z3.Boolean.mk_and ctx [expr_to_z3 e1; expr_to_z3 e2]
  | EOr (e1, e2) -> Z3.Boolean.mk_or ctx [expr_to_z3 e1; expr_to_z3 e2]
  | ENot e -> Z3.Boolean.mk_not ctx (expr_to_z3 e)
  | EDeref e -> raise (Failure "Dereference not supported for Z3")

let rec pure_to_z3 (hp : hpredicate_pure) : Expr.expr =
  match hp with
  | HTrue -> Z3.Boolean.mk_true ctx
  | HFalse -> Z3.Boolean.mk_false ctx
  | HAnd (p1, p2) -> Z3.Boolean.mk_and ctx [pure_to_z3 p1; pure_to_z3 p2]
  | HEq (e1, e2) -> Z3.Boolean.mk_eq ctx (expr_to_z3 e1) (expr_to_z3 e2)

let implies (phi:hpredicate_pure) (psi:hpredicate_pure) : bool =
  let phi_z3 = pure_to_z3 phi in
  let psi_z3 = pure_to_z3 psi in
  let solver = Solver.mk_solver ctx None in
  Solver.add solver [phi_z3; Z3.Boolean.mk_not ctx psi_z3];
  match Solver.check solver [] with
  | UNSATISFIABLE -> true
  | _ -> false
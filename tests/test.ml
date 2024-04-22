open OUnit2
open Heap_synth.Ast
open Heap_synth.Interp

let make_interpret_expr_test
    (name: string)
    (expr: expr)
    (env : (id, Heap_synth.Ast.result) Hashtbl.t)
    (heap: (int, Heap_synth.Ast.result) Hashtbl.t)
    (expected: result) =
  name >:: (fun _ ->
      assert_equal expected (interp_expr expr env heap))

let empty_env : (id, Heap_synth.Ast.result) Hashtbl.t = Hashtbl.create 10
let empty_heap : (int, Heap_synth.Ast.result) Hashtbl.t = Hashtbl.create 10
let interpret_tests = [
  make_interpret_expr_test "int" (EInt 5) empty_env empty_heap (RInt 5);
  make_interpret_expr_test "bool" (EBool true) empty_env empty_heap (RBool true);
  make_interpret_expr_test "addition" (EAdd (EInt 5, EInt 5)) empty_env empty_heap (RInt 10);
  make_interpret_expr_test "subtraction" (ESub (EInt 9, EInt 2)) empty_env empty_heap (RInt 7);
  make_interpret_expr_test "multiplication" (EMul (EInt 9, EInt 2)) empty_env empty_heap (RInt 18);
  make_interpret_expr_test "and" (EAnd (EBool true, EBool false)) empty_env empty_heap (RBool false);
  make_interpret_expr_test "or" (EOr (EBool true, EBool false)) empty_env empty_heap (RBool true);
  make_interpret_expr_test "not" (ENot (EBool true)) empty_env empty_heap (RBool false);
]

let suite = "test suite" >::: List.flatten [interpret_tests]

let _ = run_test_tt_main suite
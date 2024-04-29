open OUnit2
open Heap_synth.Ast
open Heap_synth.Interp
open Heap_synth.Rules

let make_interpret_expr_test
    (name: string)
    (expr: expr)
    (env : (id, Heap_synth.Ast.result) Hashtbl.t)
    (heap: (int, Heap_synth.Ast.result) Hashtbl.t)
    (expected: result) =
  name >:: (fun _ ->
      assert_equal expected (interp_expr expr env heap))

let make_rule_test
    (name: string)
    (rule: rule)
    (goal: goal)
    (expected: rule_result option) =
  name >:: (fun _ ->
    let result = apply_rule rule goal in
    assert_equal expected result)

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

let empty_goal = {
  pre = {
    pure = HTrue;
    spatial = HEmpty
  };
  post = {
    pure = HTrue;
    spatial = HEmpty
  };
  gamma = IdSet.empty;
  program_vars = IdSet.empty;
  universal_ghosts = IdSet.empty;
  fname = "foo";
}

let frame_rule_goal = {
  pre = {
    pure = HTrue;
    spatial = HSeparate (HEmpty, HEmpty)
  };
  post = {
    pure = HTrue;
    spatial = HSeparate (HEmpty, HEmpty)
  };
  gamma = IdSet.empty;
  program_vars = IdSet.empty;
  universal_ghosts = IdSet.empty;
  fname = "foo";
}

let frame_rule_goal2 = {
  pre = {
    pure = HTrue;
    spatial = HSeparate (HEmpty, HSeparate(HEmpty, HEmpty))
  };
  post = {
    pure = HTrue;
    spatial = HSeparate (HEmpty, HSeparate(HEmpty, HEmpty))
  };
  gamma = IdSet.empty;
  program_vars = IdSet.empty;
  universal_ghosts = IdSet.empty;
  fname = "foo";
}

let empty_goal_result = {
  subgoals = [];
  producer = CSkip;
  rule = REmp;
}

let frame_rule_result = {
  subgoals = [empty_goal];
  producer = CSkip;
  rule = RFrame;
}


let rules_tests = [
  make_rule_test "empty rule test" REmp empty_goal (Some empty_goal_result);
  make_rule_test "frame rule test" RFrame frame_rule_goal (Some frame_rule_result);
  make_rule_test "frame rule test 2" RFrame frame_rule_goal2 (Some frame_rule_result);
]

let suite = "test suite" >::: List.flatten [interpret_tests; rules_tests]

let _ = run_test_tt_main suite
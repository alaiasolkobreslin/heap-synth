open OUnit2
open Heap_synth.Ast
open Heap_synth.Interp
open Heap_synth.Rules
open Heap_synth.Synthesize

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

let make_rule_test_subgoal
    (name: string)
    (rule: rule)
    (goal: goal)
    (expected: rule_result option) =
  name >:: (fun _ ->
    let result = apply_rule rule goal in
    assert_equal expected result)

let make_rule_test_producer
    (name: string)
    (rule: rule)
    (goal: goal)
    (expected: rule_result option) =
  name >:: (fun _ ->
    let result = apply_rule rule goal in
    match result, expected with
    | Some r1, Some r2 -> 
      assert_equal r1.producer r2.producer
    | _ -> assert_equal expected result)

let make_rule_test_rule
    (name: string)
    (rule: rule)
    (goal: goal)
    (expected: rule_result option) =
  name >:: (fun _ ->
    let result = apply_rule rule goal in
    match result, expected with
    | Some r1, Some r2 -> assert_equal r1.rule r2.rule
    | _ -> assert_equal expected result)

let make_synth_test
    (name: string)
    (goal: goal)
    (expected: cmd option) =
  name >:: (fun _ ->
    let result = heap_synth goal in
    (match result, expected with
    | Some r, Some e ->
      begin
      print_endline "expected";
      print_endline (pp_cmd e);
      print_endline "actual";
      print_endline (pp_cmd r);
      end
    | _ -> print_endline "OPTIONS MISMATCH");
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

let read_rule_goal = {
  pre = {
    pure = HTrue;
    spatial = HSeparate (HPointsTo ("x", "a"), HEmpty)
  };
  post = {
    pure = HTrue;
    spatial = HEmpty
  };
  gamma = IdSet.singleton "a";
  program_vars = IdSet.empty;
  universal_ghosts = IdSet.empty;
  fname = "foo";
}

let read_rule_subgoal = {
  pre = {
    pure = HEq (EId "x0", (EId "a"));
    spatial = HSeparate (HPointsTo ("x", "x0"), HEmpty)
  };
  post = {
    pure = HEq (EId "x0", (EId "a"));
    spatial = HEmpty
  };
  gamma = IdSet.add "x0" (IdSet.singleton "a");
  program_vars = IdSet.singleton "x0";
  universal_ghosts = IdSet.empty;
  fname = "foo";
}

let write_rule_goal = {
  pre = {
    pure = HTrue;
    spatial = HPointsTo ("x", "a")
  };
  post = {
    pure = HTrue;
    spatial = HPointsTo ("x", "b")
  };
  gamma = IdSet.add "a" (IdSet.singleton "b");
  program_vars = IdSet.empty;
  universal_ghosts = IdSet.empty;
  fname = "foo";
}

let write_rule_subgoal = {
  pre = {
    pure = HTrue;
    spatial = HPointsTo ("x", "b")
  };
  post = {
    pure = HTrue;
    spatial = HPointsTo ("x", "b")
  };
  gamma = IdSet.add "a" (IdSet.singleton "b");
  program_vars = IdSet.empty;
  universal_ghosts = IdSet.empty;
  fname = "foo";
}

let swap_1_read_rule = {
  pre = {
    pure = HTrue;
    spatial = HSeparate (HPointsTo ("x", "a"), HPointsTo ("y", "b"))
  };
  post = {
    pure = HTrue;
    spatial = HSeparate (HPointsTo ("x", "b"), HPointsTo ("y", "a"))
  };
  gamma = IdSet.add "x" (IdSet.singleton "y");
  program_vars = IdSet.empty;
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_1_read_rule_subgoal = {
  pre = {
    pure = HEq (EId "x0", (EId "a"));
    spatial = HSeparate (HPointsTo ("x", "x0"), HPointsTo ("y", "b"))
  };
  post = {
    pure = HEq (EId "x0", (EId "a"));
    spatial = HSeparate (HPointsTo ("x", "b"), HPointsTo ("y", "x0"))
  };
  gamma = IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y"));
  program_vars = IdSet.singleton "x0";
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_2_read_rule = {
  pre = {
    pure = HEq (EId "x0", (EId "a"));
    spatial = HSeparate (HPointsTo ("x", "x0"), HPointsTo ("y", "b"))
  };
  post = {
    pure = HEq (EId "x0", (EId "a"));
    spatial = HSeparate (HPointsTo ("x", "b"), HPointsTo ("y", "x0"))
  };
  gamma = IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y"));
  program_vars = IdSet.singleton "x0";
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_2_read_rule_subgoal = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HSeparate (HPointsTo ("x", "x0"), HPointsTo ("y", "x1"))
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HSeparate (HPointsTo ("x", "x1"), HPointsTo ("y", "x0"))
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_3_write_rule = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HSeparate (HPointsTo ("x", "x0"), HPointsTo ("y", "x1"))
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HSeparate (HPointsTo ("x", "x1"), HPointsTo ("y", "x0"))
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_3_write_rule_subgoal = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HSeparate (HPointsTo ("x", "x1"), HPointsTo ("y", "x1"))
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HSeparate (HPointsTo ("x", "x1"), HPointsTo ("y", "x0"))
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_4_frame_rule = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HSeparate (HPointsTo ("x", "x1"), HPointsTo ("y", "x1"))
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HSeparate (HPointsTo ("x", "x1"), HPointsTo ("y", "x0"))
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_4_frame_rule_subgoal = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HPointsTo ("y", "x1")
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HPointsTo ("y", "x0")
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_5_write_rule = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HPointsTo ("y", "x1")
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HPointsTo ("y", "x0")
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_5_write_rule_subgoal = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HPointsTo ("y", "x0")
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HPointsTo ("y", "x0")
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_6_frame_rule = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HPointsTo ("y", "x0")
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HPointsTo ("y", "x0")
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_6_frame_rule_subgoal = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HEmpty
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HEmpty
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
}

let swap_7_emp_rule = {
  pre = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HEmpty
  };
  post = {
    pure = HAnd (HEq (EId "x1", EId "b"), HEq (EId "x0", (EId "a")));
    spatial = HEmpty
  };
  gamma = IdSet.add "x1" (IdSet.add "x0" (IdSet.add "x" (IdSet.singleton "y")));
  program_vars = IdSet.add "x1" (IdSet.singleton "x0");
  universal_ghosts = IdSet.empty;
  fname = "swap";
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

let read_rule_result = {
  subgoals = [read_rule_subgoal];
  producer = CLetAssign ("x0", EDeref (EId "x"));
  rule = RRead;
}

let write_rule_result = {
  subgoals = [write_rule_subgoal];
  producer = CPtrAssign (EId "x", EId "b");
  rule = RWrite;
}

let swap_1_read_rule_result = {
  subgoals = [swap_1_read_rule_subgoal];
  producer = CLetAssign ("x0", EDeref (EId "x"));
  rule = RRead;
}

let swap_2_read_rule_result = {
  subgoals = [swap_2_read_rule_subgoal];
  producer = CLetAssign ("x1", EDeref (EId "y"));
  rule = RRead;
}

let swap_3_write_rule_result = {
  subgoals = [swap_3_write_rule_subgoal];
  producer = CPtrAssign (EId "x", EId "x1");
  rule = RWrite;
}

let swap_4_frame_rule_result = {
  subgoals = [swap_4_frame_rule_subgoal];
  producer = CSkip;
  rule = RFrame;
}

let swap_5_write_rule_result = {
  subgoals = [swap_5_write_rule_subgoal];
  producer = CPtrAssign (EId "y", EId "x0");
  rule = RWrite;
}

let swap_6_frame_rule_result = {
  subgoals = [swap_6_frame_rule_subgoal];
  producer = CSkip;
  rule = RFrame;
}

let swap_7_emp_rule_result = {
  subgoals = [];
  producer = CSkip;
  rule = REmp;
}

let rules_tests = [
  make_rule_test "empty rule test" REmp empty_goal (Some empty_goal_result);
  make_rule_test "frame rule test" RFrame frame_rule_goal (Some frame_rule_result);
  make_rule_test "frame rule test 2" RFrame frame_rule_goal2 (Some frame_rule_result);
  make_rule_test "read rule test" RRead read_rule_goal (Some read_rule_result);
  make_rule_test "write rule test" RWrite write_rule_goal (Some write_rule_result);

  make_rule_test "swap 1 read rule test" RRead swap_1_read_rule (Some swap_1_read_rule_result);
  make_rule_test "swap 2 read rule test" RRead swap_2_read_rule (Some swap_2_read_rule_result);
  make_rule_test "swap 3 write rule test" RWrite swap_3_write_rule (Some swap_3_write_rule_result);
  make_rule_test "swap 4 frame rule test" RFrame swap_4_frame_rule (Some swap_4_frame_rule_result);
  make_rule_test "swap 5 write rule test" RWrite swap_5_write_rule (Some swap_5_write_rule_result);
  make_rule_test "swap 6 frame rule test" RFrame swap_6_frame_rule (Some swap_6_frame_rule_result);
  make_rule_test "swap 7 emp rule test" REmp swap_7_emp_rule (Some swap_7_emp_rule_result);
]

let synth_tests = [
  make_synth_test "swap 1" swap_1_read_rule (Some (CSeq (CLetAssign("x0", EDeref (EId "x")) ,CSeq (CLetAssign ("x1", EDeref (EId "y")) ,CSeq (CPtrAssign (EId "x", EId "x1"), CPtrAssign (EId "y", EId "x0"))))));
]

let suite = "test suite" >::: List.flatten [interpret_tests; rules_tests; synth_tests]

let _ = run_test_tt_main suite
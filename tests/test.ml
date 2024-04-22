open OUnit2
open Heap_synth.Ast
open Heap_synth.Interp

(* let compare_files f1 f2 = 
  let in_chan_f1 = open_in f1 in
  let in_chan_f2 = open_in f2 in
  let lines_f1 = ref [] in
  let lines_f2 = ref [] in
  try
    while true; do
      lines_f1 := input_line in_chan_f1 :: !lines_f1;
      lines_f2 := input_line in_chan_f2 :: !lines_f2
    done; lines_f1 = lines_f2
  with End_of_file ->
    close_in in_chan_f1;
    close_in in_chan_f2;
    lines_f1 = lines_f2

let make_lex_file_test
    (name : string)
    (input_file : string)
    (expected_file : string) : test = 
  name >:: (fun _ ->
      print_endline "uwu";
      write_out_file lex_channel "" input_file ".lexed";
      match chop_file input_file ".lexed" with
      | Some file -> assert_equal file 
                       expected_file ~cmp:compare_files
      | None -> assert_equal true false)

let lexer_tests = [
  make_lex_file_test "functions.haze" "tests/pa1/functions.haze"
    "tests/pa1/functions.lexedsol";
] *)

let make_interpret_expr_test
    (name: string)
    (expr: expr)
    (env : (id, Heap_synth.Ast.result) Hashtbl.t)
    (heap: (id, Heap_synth.Ast.result) Hashtbl.t)
    (expected: result) =
  name >:: (fun _ ->
      assert_equal expected (interp_expr expr env heap))

let empty_env : (id, Heap_synth.Ast.result) Hashtbl.t = Hashtbl.create 10
let interpret_tests = [
  make_interpret_expr_test "int" (EInt 5) empty_env empty_env (RInt 5);
  make_interpret_expr_test "bool" (EBool true) empty_env empty_env (RBool true);
  make_interpret_expr_test "addition" (EAdd (EInt 5, EInt 5)) empty_env empty_env (RInt 10);
  make_interpret_expr_test "subtraction" (ESub (EInt 9, EInt 2)) empty_env empty_env (RInt 7);
  make_interpret_expr_test "multiplication" (EMul (EInt 9, EInt 2)) empty_env empty_env (RInt 18);
  make_interpret_expr_test "and" (EAnd (EBool true, EBool false)) empty_env empty_env (RBool false);
  make_interpret_expr_test "or" (EOr (EBool true, EBool false)) empty_env empty_env (RBool true);
  make_interpret_expr_test "not" (ENot (EBool true)) empty_env empty_env (RBool false);
]

let suite = "test suite for Hazel" >::: List.flatten [interpret_tests]

let _ = run_test_tt_main suite
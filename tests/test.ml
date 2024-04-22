open OUnit2
open Main

(* let make_interpret_test *)


let interpret_tests = []

let suite = "test suite for Hazel" >::: List.flatten [interpret_tests]

let _ = run_test_tt_main suite
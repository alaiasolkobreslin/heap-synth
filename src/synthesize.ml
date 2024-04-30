open Ast
open Rules

let all_rules = [REmp; RRead; RWrite; RFrame]

let combine_cmds (cmd_lst: cmd list) : cmd =
  let rec combine_cmds' (cmd_lst: cmd list) (acc : cmd) =
    match cmd_lst with
    | [] -> print_endline "combined cmds"; print_endline (pp_cmd acc); acc
    | cmd::cmds -> combine_cmds' cmds (CSeq (acc, cmd))
  in
  combine_cmds' cmd_lst CSkip

let rec solve_subgoals goals =
  print_endline "in solve subgoals";
  let cs = List.filter_map (fun g -> synthesize g all_rules) goals in
  print_endline "got cmds";
  if List.length cs = List.length goals then Some (combine_cmds cs) else (print_endline "length mismatch"; None)

and try_alts (derivs: rule_result) (r: rule) (rs: rule list) (g: goal) : cmd option =
  match derivs.subgoals with
  | [] -> print_endline "\n\nderivs nil\n\n"; Some CSkip
  | _ ->
      match solve_subgoals derivs.subgoals with
      | None -> print_endline "naur"; None (*try_alts derivs' r rs g*)
      | Some c -> print_endline "got a command :)"; pp_cmd c |> print_endline; Some (CSeq (derivs.producer, c))

and with_rules (rs: rule list) (g: goal) : cmd option =
  (* print_endline "in with rules"; *)
  match rs with
  | [] -> print_endline "reached the end of rules"; None
  | r::rs' ->
    begin
      print_endline ("applying rule " ^ (pp_rule r));
      match apply_rule r g with
      | None -> with_rules rs' g
      | Some derivs ->
        match derivs.subgoals with 
        | [] -> Some derivs.producer
        | h::_ ->
        begin
        print_endline "\nhere original goal"; pp_goal g |> print_endline;
        print_endline "\nhere subgoal"; derivs.subgoals |> List.hd |> pp_goal |> print_endline; try_alts derivs r rs' g
        end
    end
and synthesize (g: goal) (rules:rule list): cmd option =
  with_rules rules g

let heap_synth (g: goal) : cmd option =
  synthesize g all_rules
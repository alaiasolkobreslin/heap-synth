open Ast
open Rules

let all_rules = [REmp; RRead; RWrite; RFrame]

let is_invert (r: rule) : bool =
  match r with
  | REmp -> true
  | _ -> false

let filter_comm (x: rule_result option) = x

let combine_cmds (cmd_lst: cmd list) : cmd =
  let rec combine_cmds' (cmd_lst: cmd list) (acc : cmd) =
    match cmd_lst with
    | [] -> acc
    | cmd::cmds -> combine_cmds' cmds (CSeq (acc, cmd))
  in
  combine_cmds' cmd_lst CSkip

let continuation (results: rule_result list) : cmd =
  let cmds = List.map (fun r -> r.producer) results in
  combine_cmds cmds

let rec solve_subgoals goals (k: rule_result list -> cmd) =
  let cs = List.filter_map (fun g -> synthesize g all_rules) goals in
  if List.length cs = List.length goals then Some (combine_cmds cs) else None

and try_alts (derivs: goal list) (r: rule) (rs: rule list) (g: goal) : cmd option =
  match derivs with
  | [] -> if is_invert r then None else with_rules rs g
  | goal :: derivs' ->
      match solve_subgoals [goal] continuation with
      | None -> try_alts derivs' r rs g
      | Some c -> Some c

and with_rules (rs: rule list) (g: goal) : cmd option =
  match rs with
  | [] -> None
  | r::rs' ->
    begin
      match filter_comm (apply_rule r g) with
      | None -> with_rules rs' g
      | Some derivs -> try_alts derivs.subgoals r rs' g
    end

and synthesize (g: goal) (rules:rule list): cmd option =
  with_rules rules g

let heap_synth (g: goal) : cmd option =
  synthesize g all_rules
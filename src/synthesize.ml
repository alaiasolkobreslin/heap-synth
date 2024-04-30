open Ast
open Rules

let all_rules = [REmp; RRead; RWrite; RFrame]

let rec collapse_skips cmd =
  match cmd with
  | CSkip -> CSkip
  | CSeq (CSkip, c) -> collapse_skips c
  | CSeq (c, CSkip) -> collapse_skips c
  | CSeq (c1, c2) ->
    begin
    match collapse_skips c1 with
    | CSkip -> collapse_skips c2
    | c1' -> CSeq (c1', collapse_skips c2)
    end
  | c -> c

let combine_cmds (cmd_lst: cmd list) : cmd =
  let rec combine_cmds' (cmd_lst: cmd list) (acc : cmd) =
    match cmd_lst with
    | [] -> acc
    | cmd::cmds -> combine_cmds' cmds (CSeq (acc, cmd))
  in
  combine_cmds' cmd_lst CSkip |> collapse_skips

let rec solve_subgoals goals =
  let cs = List.filter_map (fun g -> synthesize g all_rules) goals in
  if List.length cs = List.length goals then Some (combine_cmds cs) else None

and try_alts (derivs: rule_result) (r: rule) (rs: rule list) (g: goal) : cmd option =
  match derivs.subgoals with
  | [] -> Some CSkip
  | _ ->
      match solve_subgoals derivs.subgoals with
      | None -> None (*try_alts derivs' r rs g*)
      | Some c -> Some (CSeq (derivs.producer, c) |> collapse_skips)

and with_rules (rs: rule list) (g: goal) : cmd option =
  match rs with
  | [] -> None
  | r::rs' ->
    begin
      match apply_rule r g with
      | None -> with_rules rs' g
      | Some derivs ->
        match derivs.subgoals with 
        | [] -> Some derivs.producer
        | h::_ -> try_alts derivs r rs' g
    end
and synthesize (g: goal) (rules:rule list): cmd option =
  with_rules rules g

let heap_synth (g: goal) : cmd option =
  synthesize g all_rules
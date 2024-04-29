open Ast
open Rules

let all_rules = [REmp; RRead; RWrite; RFrame]

let is_invert (r: rule) : bool =
  match r with
  | REmp -> true
  | _ -> false

let filter_comm (x: rule_result option) =
  match x with
  | None -> None
  | Some rule_result -> failwith "unimplemented"


let rec solve_subgoals goals k =
  let cs = List.filter_map (fun g -> synthesize g all_rules) goals in
  if List.length cs = List.length goals then Some (k cs) else None

and try_alts derivs r rs g =
  match derivs with
  | [] -> if is_invert r then None else with_rules rs g
  | (goals, k)::derivs' ->
    begin
      match solve_subgoals goals k with
      | None -> try_alts derivs' r rs g
      | Some c -> Some c
    end

and with_rules (rs: rule list) (g: goal) =
  match rs with
  | [] -> None
  | r::rs' ->
    begin
      match filter_comm (apply_rule r g) with
      | None -> with_rules rs' g
      | Some derivs -> try_alts derivs r rs' g
    end

and synthesize (g: goal) (rules:rule list) =
  with_rules rules g

let heap_synth (g: goal) =
  synthesize g all_rules
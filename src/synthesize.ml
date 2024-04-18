open Ast
open Rules

let phases_enabled = true

let all_rules = []

let is_invert r =
  raise (Failure "Not implemented")

let filter_comm x =
  raise (Failure "Not implemented")

let next_rules g =
  raise (Failure "Not implemented")

let rec solve_subgoals goals k =
  let pick_rules g = if phases_enabled then next_rules g else all_rules in
  let cs = List.filter_map (fun g -> synthesize g (pick_rules g)) goals in
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

and with_rules rs g =
  match rs with
  | [] -> None
  | r::rs' ->
    begin
      match filter_comm (r g) with
      | None -> with_rules rs' g
      | Some derivs -> try_alts derivs r rs' g
    end

and synthesize g rules =
  with_rules rules g
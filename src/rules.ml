open Ast
open Logic

module IdSet = Set.Make(String)

type goal = {
  pre : hpredicate;
  post : hpredicate;
  gamma: IdSet.t ;
  program_vars: IdSet.t;
  universal_ghosts : IdSet.t;
  fname : string;
}

type rule = REmp | RRead | RWrite | RFrame

type rule_result = {
  subgoals: goal list;
  producer: cmd;
  rule: rule
}

let fresh_var vars =
  let rec fresh_var' i =
    let x = "x" ^ string_of_int i in
    if IdSet.mem x vars then fresh_var' (i + 1) else x
  in
  fresh_var' 0

let rec vars_prog = function
  | CLetAssign (x, _) -> IdSet.singleton x
  | CSkip
  | CPtrAssign (_, _) -> IdSet.empty (* TODO: is this correct? *)
  | CSeq (p, q)
  | CIf (_, p, q) -> IdSet.union (vars_prog p) (vars_prog q)
  | _ -> raise (Failure "Not implemented")

let rec vars_expr = function
  | EId x -> IdSet.singleton x
  | EAdd (e1, e2)
  | ESub (e1, e2)
  | EMul (e1, e2) 
  | EAnd (e1, e2)
  | EOr (e1, e2) -> IdSet.union (vars_expr e1) (vars_expr e2)
  | EDeref e 
  | ENot e -> vars_expr e
  | EInt _
  | EBool _ -> IdSet.empty

let rec vars_hpredicate_pure = function
  | HTrue
  | HFalse -> IdSet.empty
  | HAnd (p, q) -> IdSet.union (vars_hpredicate_pure p) (vars_hpredicate_pure q)
  | HEq (p, q) -> IdSet.union (vars_expr p) (vars_expr q)

let rec vars_hpredicate_spatial = function
  | HEmpty -> IdSet.empty
  | HPure e -> vars_expr e
  | HPointsTo (id1, id2) -> IdSet.union (IdSet.singleton id1) (IdSet.singleton id2)
  | HSeparate (p1, p2) -> IdSet.union (vars_hpredicate_spatial p1) (vars_hpredicate_spatial p2)
 
let ghost_vars gamma (p:hpredicate) (q:hpredicate) =
  let ghost_vars_p_pure = vars_hpredicate_pure p.pure in
  let ghost_vars_p_spatial = vars_hpredicate_spatial p.spatial in
  IdSet.diff (IdSet.union ghost_vars_p_pure ghost_vars_p_spatial) gamma

let existential_vars gamma (p:hpredicate) (q:hpredicate) =
  let vars_p_pure = vars_hpredicate_pure p.pure in
  let vars_p_spatial = vars_hpredicate_spatial p.spatial in
  let vars_q_pure = vars_hpredicate_pure q.pure in
  let vars_q_spatial = vars_hpredicate_spatial q.spatial in
  let vars_p = IdSet.union vars_p_pure vars_p_spatial in
  let vars_q = IdSet.union vars_q_pure vars_q_spatial in
  IdSet.diff vars_q (IdSet.union gamma vars_p)

let rec find_points_to_heaplet (pre: hpredicate_spatial) = 
  match pre with
  | HPointsTo (x, e) -> Some (HPointsTo (x, e), HEmpty)
  | HSeparate (p1, p2) ->
      (match find_points_to_heaplet p1 with
      | None -> None
      | Some (h, p) -> Some (h, HSeparate (p, p2)))
  | _ -> None

let apply_emp_rule (goal:goal) =
  if goal.pre.spatial = HEmpty && goal.post.spatial = HEmpty then
    if existential_vars goal.gamma goal.pre goal.post = IdSet.empty then
      if implies goal.pre.pure goal.post.pure then
        Some { subgoals = []; producer = CSkip; rule = REmp}
      else
        None
    else
      None
  else
    None

let apply_read_rule (goal:goal) =
  match find_points_to_heaplet goal.pre.spatial with
  | None -> None
  | Some (HPointsTo (x, e), p) ->
      let y = fresh_var goal.program_vars in
      let new_program_vars = IdSet.add y goal.program_vars in
      let new_pre_pure = HAnd (HEq (EId y, EId e), goal.pre.pure) in
      let new_pre_spatial = subst_spatial goal.pre.spatial x y in
      let new_pre = { pure = new_pre_pure; spatial = new_pre_spatial } in
      let new_post_pure = HAnd (HEq (EId y, EId e), goal.post.pure) in
      let new_post_spatial = subst_spatial goal.post.spatial x y in
      let new_post = { pure = new_post_pure; spatial = new_post_spatial } in
      let new_goal = { pre = new_pre; post = new_post; gamma = goal.gamma; program_vars = new_program_vars; universal_ghosts = goal.universal_ghosts; fname = goal.fname } in
      Some { subgoals = [new_goal]; producer = CLetAssign (y, EDeref (EId x)); rule = RRead }
  | _ -> failwith "invalid return value for find_points_to_heaplet"


let apply_write_rule (goal:goal) =
  match find_points_to_heaplet goal.pre.spatial, find_points_to_heaplet goal.post.spatial with
  | Some (HPointsTo (x, e'), p), Some (HPointsTo (y, e), q) ->
      if x != y then None else
      let new_pre_spatial = HSeparate (HPointsTo (x, e), p) in
      let new_pre = { goal.pre with spatial = new_pre_spatial } in
      let new_goal = { goal with pre = new_pre} in
      Some { subgoals = [new_goal]; producer = CPtrAssign (EId x, EId e); rule = RWrite }
  | _ -> None

(* let apply_frame_rule (goal:goal) =
  if not (profiles_match goal.pre.spatial goal.post.spatial true) then
    None
  else
    let is_match = true in
    match find_matching_heaplets (fun x -> true) is_match goal.pre.spatial goal.post.spatial with
    | None -> None
    | Some (h_pre, h_post) ->
        let new_pre_spatial = remove_spatial_predicate goal.pre.spatial h_pre in
        let new_post_spatial = remove_spatial_predicate goal.post.spatial h_post in
        let new_pre = { goal.pre with spatial = new_pre_spatial } in
        let new_post = { goal.post with spatial = new_post_spatial } in
        let new_goal = { goal with pre = new_pre; post = new_post } in
        Some { subgoals = [new_goal]; producer = CSkip; rule = RFrame } *)

let apply_frame_rule (goal:goal) =
  let rec find_matching_separate (p1: hpredicate_spatial) (p2: hpredicate_spatial) =
    match p1, p2 with
    | HSeparate (p, r1), HSeparate (q, r2) ->
        begin
        if r1 = r2 then Some (p, q, r1) else
          match find_matching_separate r1 r2 with
          | Some (p', q', r) -> Some (HSeparate (p, HSeparate(p', r)), HSeparate (q, HSeparate(q', r)), r)
          | None -> None
        end
    | _ -> None in
  match find_matching_separate goal.pre.spatial goal.post.spatial with
  | None -> None
  | Some (p, q, r) ->
      let new_pre_spatial = p in
      let new_post_spatial = q in
      let new_pre = { goal.pre with spatial = new_pre_spatial } in
      let new_post = { goal.post with spatial = new_post_spatial } in
      let new_goal = { goal with pre = new_pre; post = new_post } in
      Some { subgoals = [new_goal]; producer = CSkip; rule = RFrame }

let apply_rule (rule:rule) (goal:goal) =
  match rule with
  | REmp -> apply_emp_rule goal
  | RRead -> apply_read_rule goal
  | RWrite -> apply_write_rule goal
  | RFrame -> apply_frame_rule goal

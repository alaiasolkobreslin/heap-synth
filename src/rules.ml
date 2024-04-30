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

let pp_goal goal =
  let pp_id_set s = IdSet.fold (fun x acc -> acc ^ x ^ ", ") s "" in
  "Pre: " ^ pp_hpredicate goal.pre ^ 
  "\nPost: " ^ pp_hpredicate goal.post ^ 
  "\nGamma: " ^ pp_id_set goal.gamma ^ 
  "\nProgram vars: " ^ pp_id_set goal.program_vars ^ 
  "\nUniversal ghosts: " ^ pp_id_set goal.universal_ghosts ^
  "\nFunction name: " ^ goal.fname

let pp_rule = function
  | REmp -> "Emp"
  | RRead -> "Read"
  | RWrite -> "Write"
  | RFrame -> "Frame"

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

let rec find_points_to_heaplet_read (pre: hpredicate_spatial) program_vars = 
  match pre with
  | HPointsTo (x, e) -> 
      begin
        match IdSet.find_opt e program_vars with
        | None -> Some (HPointsTo (x, e), HEmpty)
        | _ -> None
      end
  | HSeparate (p1, p2) ->
      let p1_points_to = find_points_to_heaplet_read p1 program_vars in
      let p2_points_to = find_points_to_heaplet_read p2 program_vars in
      begin
      match p1_points_to, p2_points_to with
        | None, None -> None
        | Some (h, HEmpty), _ -> Some (h, p2)
        | _, Some (h, HEmpty) -> Some (h, p1)
        | Some (h, p), _ -> Some (h, HSeparate (p, p2))
        | _, Some (h, p) -> Some (h, HSeparate (p1, p))
      end
  | _ -> None

let rec find_points_to_heaplet_write (pre: hpredicate_spatial) program_vars = 
  match pre with
  | HPointsTo (x, e) -> Some (HPointsTo (x, e), HEmpty)
  | HSeparate (p1, p2) ->
      let p1_points_to = find_points_to_heaplet_write p1 program_vars in
      let p2_points_to = find_points_to_heaplet_write p2 program_vars in
      begin
      match p1_points_to, p2_points_to with
        | None, None -> None
        | Some (h, HEmpty), _ -> Some (h, p2)
        | _, Some (h, HEmpty) -> Some (h, p1)
        | Some (h, p), _ -> Some (h, HSeparate (p, p2))
        | _, Some (h, p) -> Some (h, HSeparate (p1, p))
      end
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
  match find_points_to_heaplet_read goal.pre.spatial goal.program_vars with
  | None -> None
  | Some (HPointsTo (x, e), p) ->
      let y = fresh_var goal.program_vars in
      let new_program_vars = IdSet.add y goal.program_vars in
      let new_pre_pure = if goal.pre.pure = HTrue then (HEq (EId y, EId e)) 
        else HAnd (HEq (EId y, EId e), goal.pre.pure) in
      let new_pre_spatial = subst_spatial goal.pre.spatial e y in
      let new_pre = { pure = new_pre_pure; spatial = new_pre_spatial } in
      let new_post_pure = if goal.post.pure = HTrue then (HEq (EId y, EId e)) 
        else HAnd (HEq (EId y, EId e), goal.post.pure) in
      let new_post_spatial = subst_spatial goal.post.spatial e y in
      let new_post = { pure = new_post_pure; spatial = new_post_spatial } in
      let new_gamma = IdSet.add y goal.gamma in
      let new_goal = { pre = new_pre; post = new_post; gamma = new_gamma; program_vars = new_program_vars; universal_ghosts = goal.universal_ghosts; fname = goal.fname } in
      print_endline "result of read rule applied";
      pp_goal new_goal |> print_endline;
      let result = { subgoals = [new_goal]; producer = CLetAssign (y, EDeref (EId x)); rule = RRead } in
      if new_goal = goal then None else
      Some result
  | _ -> failwith "invalid return value for find_points_to_heaplet"


let apply_write_rule (goal:goal) =
  match find_points_to_heaplet_write goal.pre.spatial goal.program_vars, 
        find_points_to_heaplet_write goal.post.spatial goal.program_vars with
  | Some (HPointsTo (x, e'), p), Some (HPointsTo (y, e), q) ->
      if x != y then None else
        let new_pre_spatial = if p = HEmpty then HPointsTo (x, e) else HSeparate (HPointsTo (x, e), p) in
      (* let new_pre_spatial = HSeparate (HPointsTo (x, e), p) in *)
      let new_pre = { goal.pre with spatial = new_pre_spatial } in
      let new_goal = { goal with pre = new_pre} in
      if new_goal = goal then None else
      Some { subgoals = [new_goal]; producer = CPtrAssign (EId x, EId e); rule = RWrite }
  | _ -> None

let apply_frame_rule (goal:goal) =
  let rec find_matching_separate (p1: hpredicate_spatial) (p2: hpredicate_spatial) =
    match p1, p2 with
    | HSeparate (p, r1), HSeparate (q, r2) ->
        begin
        if r1 = r2 then Some (p, q, r1) else
        if p = q then Some (r1, r2, p) else
          match find_matching_separate r1 r2 with
          | Some (p', q', r) -> Some (HSeparate (p, HSeparate(p', r)), HSeparate (q, HSeparate(q', r)), r)
          | None -> None
        end
    | _ -> None in
  if goal.pre.spatial = goal.post.spatial then 
    begin
      let new_pre = { goal.pre with spatial = HEmpty } in
      let new_post = { goal.post with spatial = HEmpty } in
      let new_goal = { goal with pre = new_pre; post = new_post } in
      Some { subgoals = [new_goal]; producer = CSkip; rule = RFrame }
    end
  else
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

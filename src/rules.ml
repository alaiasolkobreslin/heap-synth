open Ast

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

let rec vars_prog = function
  | CLetAssign (x, _) -> IdSet.singleton x
  | CSkip
  | CPtrAssign (_, _) -> IdSet.empty (* TODO: is this correct? *)
  | CSeq (p, q)
  | CIf (_, p, q) -> IdSet.union (vars_prog p) (vars_prog q)
  | CWhile (_, p) -> vars_prog p
  | _ -> raise (Failure "Not implemented")

let rec vars_expr = function
  | EId x -> IdSet.singleton x
  | EAdd (e1, e2)
  | ESub (e1, e2)
  | EMul (e1, e2) 
  | EAnd (e1, e2)
  | EOr (e1, e2) -> IdSet.union (vars_expr e1) (vars_expr e2)
  | ERef e
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

(*   
  def profilesMatch(pre: SFormula, post: SFormula, exact: Boolean): Boolean = {
    if (exact) pre.profile.apps == post.profile.apps else multiSubset(post.profile.apps, pre.profile.apps)
  } *)

let profiles_match pre post exact = failwith "unimplemented"

let implies phi psi =
  raise (Failure "implies: not implemented")

let apply_emp_rule (goal:goal) =
  let pre = goal.pre in
  let post = goal.post in
  if pre.spatial = HEmpty && post.spatial = HEmpty then
    if existential_vars goal.gamma goal.pre goal.post = IdSet.empty then
      if implies pre.pure post.pure then
        Some CSkip
      else
        None
    else
      None
  else
    None

let apply_read_rule (goal:goal) = failwith "unimplemented"

let apply_write_rule (goal:goal) = failwith "unimplemented"

let apply_frame_rule (goal:goal) =
  let pre = goal.pre in
  let post = goal.post in
  failwith "unimplemented"

let apply_rule rule (goal:goal) =
  match rule with
  | REmp -> apply_emp_rule goal
  | RRead -> apply_read_rule goal
  | RWrite -> apply_write_rule goal
  | RFrame -> apply_frame_rule goal

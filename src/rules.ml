open Ast

module IdSet = Set.Make(String)

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
  | EMul (e1, e2) -> IdSet.union (vars_expr e1) (vars_expr e2)
  | ERef e
  | EDeref e -> vars_expr e
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
  let ghost_vars_p_pure = vars_hpredicate_pure (fst p) in
  let ghost_vars_p_spatial = vars_hpredicate_spatial (snd p) in
  (* let vars_q_pure = vars_hpredicate_pure (fst q) in *)
  (* let vars_q_spatial = vars_hpredicate_spatial (snd q) in *)
  IdSet.diff (IdSet.union ghost_vars_p_pure ghost_vars_p_spatial) gamma

let existential_vars gamma (p:hpredicate) (q:hpredicate) =
  let vars_p_pure = vars_hpredicate_pure (fst p) in
  let vars_p_spatial = vars_hpredicate_spatial (snd p) in
  let vars_q_pure = vars_hpredicate_pure (fst q) in
  let vars_q_spatial = vars_hpredicate_spatial (snd q) in
  let vars_p = IdSet.union vars_p_pure vars_p_spatial in
  let vars_q = IdSet.union vars_q_pure vars_q_spatial in
  IdSet.diff vars_q (IdSet.union gamma vars_p)

let implies phi psi =
  raise (Failure "implies: not implemented")

let emp_rule htransform =
  let pre_p_pure, pre_p_spatial = htransform.hpred_pre in
  let post_p_pure, post_p_spatial = htransform.hpred_post in
  match pre_p_spatial, post_p_spatial with
  | HEmpty, HEmpty ->
    if existential_vars htransform.env htransform.hpred_pre htransform.hpred_post = IdSet.empty then
      if implies pre_p_pure post_p_pure then
        Some CSkip
      else
        None
    else
      None
  | _ -> None

(* let read_rule htransform =
  let pre_p_pure, pre_p_spatial = htransform.hpred_pre in
  let post_p_pure, post_p_spatial = htransform.hpred_post in
  match pre_p_spatial with
  | HSeparate(HPointsTo(id1, id2), p) ->
    begin
      if IdSet.mem id2 (ghost_vars htransform.env htransform.hpred_pre htransform.hpred_post) then
        None
      else
        if implies pre_p_pure post_p_pure then
          Some (CLetAssign (id2, EDeref (ERef id1)))
        else
          None
    end
  | _ -> None *)

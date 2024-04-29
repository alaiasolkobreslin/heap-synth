
type id = string
module IdSet = Set.Make(String)

type env = IdSet.t

type expr =
  | EInt of int
  | EBool of bool
  | EId of id
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | ENot of expr
  | EDeref of expr

type cmd =
  | CSkip
  | CLetAssign of id * expr (* load *)
  | CPtrAssign of expr * expr (* store *)
  | CSeq of cmd * cmd
  | CIf of expr * cmd * cmd
  | CAlloc of id * expr
  | CFree of expr
  | CHole

type prog = (* location arguments *) id list * (* program body *) cmd

type hpredicate_spatial =
  | HEmpty
  | HPure of expr
  | HPointsTo of id * id
  | HSeparate of hpredicate_spatial * hpredicate_spatial

type hpredicate_pure =
  | HTrue
  | HFalse
  | HAnd of hpredicate_pure * hpredicate_pure
  | HEq of expr * expr

type hpredicate = 
{
  pure: hpredicate_pure;
  spatial: hpredicate_spatial;
}

type heap_transform =
  {
    env: env;
    hpred_pre: hpredicate;
    hpred_post: hpredicate;
  }

type result =
  | RBool of bool
  | RInt of int
  | RError

let subst_spatial pred id1 id2 =
  let rec subst_spatial' pred = match pred with
    | HEmpty -> HEmpty
    | HPure e -> HPure e
    | HPointsTo (id, id') -> HPointsTo (id, if id' = id1 then id2 else id')
    | HSeparate (pred1, pred2) -> HSeparate (subst_spatial' pred1, subst_spatial' pred2)
  in
  subst_spatial' pred

let rec pp_expr expr = 
  match expr with
  | EInt i -> string_of_int i
  | EBool b -> string_of_bool b
  | EId id -> id
  | EAdd (e1, e2) -> (pp_expr e1) ^ " + " ^ (pp_expr e2)
  | ESub (e1, e2) -> (pp_expr e1) ^ " - " ^ (pp_expr e2)
  | EMul (e1, e2) -> (pp_expr e1) ^ " * " ^ (pp_expr e2)
  | EAnd (e1, e2) -> (pp_expr e1) ^ " && " ^ (pp_expr e2)
  | EOr (e1, e2) -> (pp_expr e1) ^ " || " ^ (pp_expr e2)
  | ENot e -> "!" ^ (pp_expr e)
  | EDeref e -> "*" ^ (pp_expr e)

and pp_cmd cmd =
  match cmd with
  | CSkip -> "skip"
  | CLetAssign (id, e) -> id ^ " := " ^ (pp_expr e)
  | CPtrAssign (e1, e2) -> "*" ^ (pp_expr e1) ^ " := " ^ (pp_expr e2)
  | CSeq (cmd1, cmd2) -> (pp_cmd cmd1) ^ "; " ^ (pp_cmd cmd2)
  | CIf (e, cmd1, cmd2) -> "if " ^ (pp_expr e) ^ " then " ^ (pp_cmd cmd1) ^ " else " ^ (pp_cmd cmd2)
  | CAlloc (id, e) -> "alloc " ^ id ^ " := " ^ (pp_expr e)
  | CFree e -> "free " ^ (pp_expr e)
  | CHole -> "hole"

let pp_hpredicate hpred =
  let rec pp_hpredicate_spatial pred = match pred with
    | HEmpty -> "emp"
    | HPure e -> (pp_expr e)
    | HPointsTo (id, id') -> id ^ " -> " ^ id'
    | HSeparate (pred1, pred2) -> (pp_hpredicate_spatial pred1) ^ " * " ^ (pp_hpredicate_spatial pred2)
  in
  let rec pp_hpredicate_pure pred = match pred with
    | HTrue -> "true"
    | HFalse -> "false"
    | HAnd (pred1, pred2) -> (pp_hpredicate_pure pred1) ^ " /\\ " ^ (pp_hpredicate_pure pred2)
    | HEq (e1, e2) -> (pp_expr e1) ^ " = " ^ (pp_expr e2)
  in
  (pp_hpredicate_pure hpred.pure) ^ " * " ^ (pp_hpredicate_spatial hpred.spatial)
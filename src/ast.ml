
type id = string
module IdSet = Set.Make(String)

type env = IdSet.t

type expr =
  | EInt of int
  | EBool of int
  | EId of id
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | ENot of expr
  | ERef of expr
  | EDeref of expr

type cmd =
  | CSkip
  | CLetAssign of id * expr (* load *)
  | CPtrAssign of expr * expr (* store *)
  | CSeq of cmd * cmd
  | CIf of expr * cmd * cmd
  | CWhile of expr * cmd
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

type hpredicate = hpredicate_pure * hpredicate_spatial

type heap_transform =
  {
    env: env;
    hpred_pre: hpredicate;
    hpred_post: hpredicate;
  }

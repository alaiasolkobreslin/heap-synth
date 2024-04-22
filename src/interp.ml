open Ast
open Hashtbl

let counter = ref 0

let new_var () =
  let v = !counter in
  counter := !counter + 1;
  v

let rec interp_expr expr env heap = match expr with
  | EInt i -> i
  | EAdd (e1, e2) -> interp_expr e1 env heap + interp_expr e2 env heap
  | ESub (e1, e2) -> interp_expr e1 env heap - interp_expr e2 env heap
  | EMul (e1, e2) -> interp_expr e1 env heap * interp_expr e2 env heap
  | EBool b -> b
  | EOr (e1, e2) -> if interp_expr e1 env heap = 1 || interp_expr e2 env heap = 1 then 1 else 0
  | EAnd (e1, e2) -> if interp_expr e1 env heap = 1 && interp_expr e2 env heap = 1 then 1 else 0
  | ENot e -> if interp_expr e env heap = 1 then 0 else 1
  | EId x -> Hashtbl.find env x  (*List.assoc x env*)
  (* | ERef x -> Hashtbl.add env x (new_var ()); Hashtbl.add heap (Hashtbl.find env x) (interp_expr x env heap); Hashtbl.find env x *)
  | EDeref x -> failwith "Not implemented"
  | ERef x -> failwith "Not implemented"


(* let initial_env = create 10
let initial_heap = create 10 *)
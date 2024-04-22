open Ast
open Hashtbl

let counter = ref 0

let new_var () =
  let v = !counter in
  counter := !counter + 1;
  v

let unwrap_int = function
  | RInt i -> i
  | _ -> failwith "Expected an integer"

let unwrap_bool = function
  | RBool b -> b
  | _ -> failwith "Expected a boolean"

let rec interp_expr expr env heap : result = match expr with
  | EInt i -> RInt i
  | EAdd (e1, e2) -> RInt ((interp_expr e1 env heap |> unwrap_int) + (interp_expr e2 env heap |> unwrap_int))
  | ESub (e1, e2) -> RInt ((interp_expr e1 env heap |> unwrap_int) - (interp_expr e2 env heap |> unwrap_int))
  | EMul (e1, e2) -> RInt ((interp_expr e1 env heap |> unwrap_int) * (interp_expr e2 env heap |> unwrap_int))
  | EBool b -> RBool b
  | EOr (e1, e2) -> RBool (interp_expr e1 env heap |> unwrap_bool || interp_expr e2 env heap |> unwrap_bool)
  | EAnd (e1, e2) -> RBool (interp_expr e1 env heap |> unwrap_bool && interp_expr e2 env heap |> unwrap_bool)
  | ENot e -> RBool (interp_expr e env heap |> unwrap_bool |> not)
  | EId x -> Hashtbl.find env x  (*List.assoc x env*)
  (* | ERef x -> Hashtbl.add env x (new_var ()); Hashtbl.add heap (Hashtbl.find env x) (interp_expr x env heap); Hashtbl.find env x *)
  | EDeref x -> failwith "Not implemented"
  | ERef x -> failwith "Not implemented"


(* let initial_env = create 10
let initial_heap = create 10 *)
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
  | EId x -> Hashtbl.find env x
  | EDeref (EId x) ->
      let loc = Hashtbl.find env x in
      Hashtbl.find heap (unwrap_int loc)
  | EDeref _ -> failwith "Attempted to deference a non-variable"

let rec interp_cmd cmd env heap : unit = match cmd with
  | CSkip -> ()
  | CLetAssign (x, e) -> Hashtbl.replace env x (interp_expr e env heap)
  | CPtrAssign (EId x, e2) ->
    begin
      match Hashtbl.find_opt env x with
      | Some loc -> Hashtbl.replace heap (unwrap_int loc) (interp_expr e2 env heap)
      | None -> let v = new_var () in
                Hashtbl.replace env x (RInt v);
                Hashtbl.replace heap v (interp_expr e2 env heap)
    end
  | CSeq (c1, c2) -> interp_cmd c1 env heap; interp_cmd c2 env heap
  | CIf (e, c1, c2) -> if interp_expr e env heap |> unwrap_bool then interp_cmd c1 env heap else interp_cmd c2 env heap
  | CAlloc (x, e) -> failwith "Not implemented"
  | CFree e -> failwith "Not implemented"
  | CHole -> failwith "Hole not filled"
  | CPtrAssign _ -> failwith "Attempted to assign to a non-variable"

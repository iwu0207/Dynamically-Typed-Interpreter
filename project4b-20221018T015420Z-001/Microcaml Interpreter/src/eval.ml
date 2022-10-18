open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
  |Value(h) -> h
  |ID h -> lookup env h
  |Not h -> let x = (eval_expr env h) in begin match x with
     |Bool true -> Bool (false)
     |Bool false -> Bool (true)
     |_ -> raise (TypeError ("Expected type bool"))
    end
  |Binop (op, v1, v2) -> let v3 = (eval_expr env v1) in let v4 = (eval_expr env v2) in begin match op with
    |Add -> begin match v3 with
      |Int x -> begin match v4 with
        |Int x2 -> Int (x+x2) 
        |_ -> raise (TypeError ("Expected type int"))
      end
      |_ -> raise (TypeError ("Expected type int"))
    end
    |Mult -> begin match v3 with
      |Int x -> begin match v4 with
        |Int x2 -> Int (x*x2) 
        |_ -> raise (TypeError ("Expected type int"))
      end
      |_ -> raise (TypeError ("Expected type int"))
    end
    |Div -> begin match v3 with
      |Int x -> begin match v4 with
        |Int x2 -> if x2 = 0 then raise(DivByZeroError) else Int (x/x2) 
        |_ -> raise (TypeError ("Expected type int"))
      end
      |_ -> raise (TypeError ("Expected type int"))
    end
    |Sub -> begin match v3 with
      |Int x -> begin match v4 with
        |Int x2 -> Int (x-x2) 
        |_ -> raise (TypeError ("Expected type int"))
      end
      |_ -> raise (TypeError ("Expected type int"))
    end
    |Greater -> begin match v3 with
      |Int x -> begin match v4 with
        |Int x2 -> Bool (x > x2) 
        |_ -> raise (TypeError ("Expected type int"))
      end
      |_ -> raise (TypeError ("Expected type int"))
    end
    |Less -> begin match v3 with
      |Int x -> begin match v4 with
        |Int x2 -> Bool (x < x2) 
        |_ -> raise (TypeError ("Expected type int"))
      end
      |_ -> raise (TypeError ("Expected type int"))
    end
    |GreaterEqual -> begin match v3 with
      |Int x -> begin match v4 with
        |Int x2 -> Bool (x >= x2) 
        |_ -> raise (TypeError ("Expected type int"))
      end
      |_ -> raise (TypeError ("Expected type int"))
    end
    |LessEqual -> begin match v3 with
      |Int x -> begin match v4 with
        |Int x2 -> Bool (x <= x2) 
        |_ -> raise (TypeError ("Expected type int"))
      end
      |_ -> raise (TypeError ("Expected type int"))
    end
    |Concat -> begin match v3 with
      |String x -> begin match v4 with
        |String x2 -> String (x ^ x2) 
        |_ -> raise (TypeError ("Expected type string"))
      end
      |_ -> raise (TypeError ("Expected type string"))
    end
    |Equal -> begin match v3 with
      |Bool x -> begin match v4 with
        |Bool x2 -> Bool (x = x2)
        |_ -> raise (TypeError ("Cannot compare types"))
      end
      |Int x -> begin match v4 with
        |Int x2 -> Bool (x = x2)
        |_ -> raise (TypeError ("Cannot compare types"))
      end
      |String x -> begin match v4 with
        |String x2 -> Bool (x = x2)
        |_ -> raise (TypeError ("Cannot compare types"))
      end      
      |_ -> raise (TypeError ("Cannot compare types"))
    end
    |NotEqual -> begin match v3 with
      |Bool x -> begin match v4 with
        |Bool x2 -> Bool (x <> x2)
        |_ -> raise (TypeError ("Cannot compare types"))
      end
      |Int x -> begin match v4 with
        |Int x2 -> Bool (x <> x2)
        |_ -> raise (TypeError ("Cannot compare types"))
      end
      |String x -> begin match v4 with
        |String x2 -> Bool (x <> x2)
        |_ -> raise (TypeError ("Cannot compare types"))
      end
      |_ -> raise (TypeError ("Cannot compare types"))
    end
    |Or -> begin match v3 with
      |Bool x -> begin match v4 with
        |Bool x2 -> Bool (x || x2) 
        |_ -> raise (TypeError ("Expected type bool"))
      end
      |_ -> raise (TypeError ("Expected type bool"))
    end
    |And -> begin match v3 with
      |Bool x -> begin match v4 with
        |Bool x2 -> Bool (x && x2) 
        |_ -> raise (TypeError ("Expected type bool"))
      end
      |_ -> raise (TypeError ("Expected type bool"))
    end
  end
  |If (x, y, z) -> let guard = (eval_expr env x) in begin match guard with
    |Bool true -> eval_expr env y
    |Bool false -> eval_expr env z
    |_ -> raise (TypeError ("Expected type bool"))
  end
  |Let (w, x, y, z) -> if x = false then eval_expr (extend env w (eval_expr env y)) z
    else let env2 = (extend_tmp env w) in let v = eval_expr env2 y in
    update env2 w v;
    eval_expr env2 z
  |Fun (x, y) -> Closure(env, x, y)
  |FunctionCall (x, y) -> let b = (eval_expr env x) in let v = (eval_expr env y) in
      begin match b with
      |Closure(f, c, e) -> eval_expr (f@[(c, ref(v))]) e
      |_ -> raise (TypeError ("Not a function"))
      end
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
  |Def (x, y) -> let z = (eval_expr env y) in ((env@[(x, {contents = z})]), Some(z))
  |Expr(x) -> (env, Some(eval_expr env x))
  |NoOp -> (env, None)

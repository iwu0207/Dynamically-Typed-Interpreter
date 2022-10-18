open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks =
  match lookahead toks with
  |None -> raise (InvalidInputException "InvalidInputException")
  |Some h -> begin match h with
    |Tok_Let -> let rem_toks = match_token toks Tok_Let in 
      begin match parse_rec rem_toks with
      |(a1, a2) -> begin match lookahead a1 with
        |None -> raise (InvalidInputException "InvalidInputException")
        |Some h2 -> begin match h2 with
          |Tok_ID h3 -> let rem_toks2 = match_token (match_token a1 (Tok_ID h3)) Tok_Equal in 
            begin match parse_expr rem_toks2 with
            |(a3, a4) -> begin match parse_expr (match_token a3 Tok_In) with
              |(a5, a6) ->(a5, Let(h3, a2, a4, a6))
              end
            end 
          |_ -> raise (InvalidInputException "parse_expr")          
          end
        end
    end
    |Tok_Fun -> let rem_toks = match_token toks Tok_Fun in
      begin match lookahead rem_toks with
      |None -> raise (InvalidInputException "parse_fun")
      |Some h2 -> let rem_toks2 = match_token rem_toks (h2) in 
        let rem_toks3 = match_token rem_toks2 Tok_Arrow in begin match parse_expr rem_toks3 with
        |(a1, a2) -> begin match h2 with
          |Tok_ID h3 -> (a1, Fun(h3, a2))
          |_ -> raise (InvalidInputException "parse_fun")
          end
        end
      end
    |Tok_If -> let rem_toks = match_token toks Tok_If in
      begin match parse_expr rem_toks with
      |(a1, a2) -> let rem_toks2 = match_token a1 Tok_Then in begin match parse_expr rem_toks2 with
        |(a3, a4) -> let rem_toks3 = match_token a3 Tok_Else in begin match parse_expr rem_toks3 with
          |(a5, a6) -> (a5, If(a2, a4, a6))
          end
        end
      end
    |_ -> begin match lookahead (match_token toks h) with 
      |None -> parse_and toks
      |Some h2 -> if h2 <> Tok_Or then (parse_and toks) else 
        begin match parse_and toks with
        |(a1, a2) -> let rem_toks = (match_token a1 Tok_Or) in begin match parse_expr rem_toks with
          |(a3, a4) -> (a3, Binop(Or, a2, a4))
          end
        end
      end 
    end

and parse_rec toks =
  match toks with
  |[] -> ([], false)
  |h::t -> if h = Tok_Rec then (t, true) else (toks, false)

and parse_and toks = match toks with
  |[] -> raise (InvalidInputException "parse_and")
  |h::t -> begin match lookahead (match_token toks h) with 
      |None -> parse_equal toks
      |Some h2 -> if h2 <> Tok_And then (parse_equal toks) else 
        begin match parse_equal toks with
        |(a1, a2) -> let rem_toks = (match_token a1 Tok_And) in begin match parse_and rem_toks with
          |(a3, a4) -> (a3, Binop(And, a2, a4))
        end
      end 
    end

and parse_equal toks = match toks with
  |[] -> raise (InvalidInputException "parse_equal")
  |h::t -> begin match lookahead (match_token toks h) with 
      |None -> parse_rel toks
      |Some h2 -> if (h2 <> Tok_Equal) && (h2 <> Tok_NotEqual) then (parse_rel toks) else 
        begin match parse_rel toks with
        |(a1, a2) -> let oper = (equal_op a1) in let rem_toks = (match_token a1 oper) in begin match parse_equal rem_toks with
          |(a3, a4) -> if oper = Tok_Equal then (a3, Binop(Equal, a2, a4)) else (a3, Binop(NotEqual, a2, a4))
          end
        end
      end

and equal_op toks =
  match toks with
  |[] -> raise (InvalidInputException "equal_op")
  |h::t -> if h = Tok_Equal then Tok_Equal else if h = Tok_NotEqual then Tok_NotEqual
  else raise (InvalidInputException "InvalidInputException")


and parse_rel toks = match toks with
  |[] -> raise (InvalidInputException "parse_rel")
  |h::t -> begin match lookahead (match_token toks h) with 
      |None -> parse_add toks
      |Some h2 -> if (h2 <> Tok_Less) && (h2 <> Tok_Greater) && (h2 <> Tok_GreaterEqual) && (h2 <> Tok_LessEqual) then (parse_add toks) else 
        begin match parse_add toks with
        |(a1, a2) -> let oper = (rel_op a1) in let rem_toks = (match_token a1 oper) in begin match parse_rel rem_toks with
          |(a3, a4) -> if oper = Tok_Greater then (a3, Binop(Greater, a2, a4)) else if oper = Tok_Less then (a3, Binop(Less, a2, a4))
          else if oper = Tok_GreaterEqual then (a3, Binop(GreaterEqual, a2, a4)) else (a3, Binop(LessEqual, a2, a4))
          end
        end
      end
  

and rel_op toks = match toks with
  |[] -> raise (InvalidInputException "rel_op")
  |h::t -> if h = Tok_Greater then Tok_Greater else if h = Tok_Less then Tok_Less
  else if h = Tok_GreaterEqual then Tok_GreaterEqual else if h = Tok_LessEqual then Tok_LessEqual
  else raise (InvalidInputException "InvalidInputException")


and parse_add toks = match toks with
  |[] -> raise (InvalidInputException "parse_add")
  |h::t -> begin match lookahead (match_token toks h) with 
      |None -> parse_multi toks
      |Some h2 -> if (h2 <> Tok_Add) && (h2 <> Tok_Sub) then (parse_multi toks) else 
        begin match parse_multi toks with
        |(a1, a2) -> let oper = (add_op a1) in let rem_toks = (match_token a1 oper) in begin match parse_add rem_toks with
          |(a3, a4) -> if oper = Tok_Add then (a3, Binop(Add, a2, a4)) else (a3, Binop(Sub, a2, a4))
          end
        end
      end


and add_op toks = 
  match toks with
  |[] -> raise (InvalidInputException "add_op")
  |h::t -> if h = Tok_Add then Tok_Add else if h = Tok_Sub then Tok_Sub
  else raise (InvalidInputException "InvalidInputException")


and parse_multi toks = match toks with
  |[] -> raise (InvalidInputException "parse_multi")
  |h::t -> begin match lookahead (match_token toks h) with 
      |None -> parse_concat toks
      |Some h2 -> if (h2 <> Tok_Mult) && (h2 <> Tok_Div) then (parse_concat toks) else 
        begin match parse_concat toks with
        |(a1, a2) -> let oper = (multi_op a1) in let rem_toks = (match_token a1 oper) in begin match (parse_multi rem_toks) with
          |(a3, a4) -> if oper = Tok_Mult then (a3, Binop(Mult, a2, a4)) else (a3, Binop(Div, a2, a4))
      end
    end 
  end

and multi_op toks = 
  match toks with
  |[] -> raise (InvalidInputException "multi_op")
  |h::t -> if h = Tok_Mult then Tok_Mult else if h = Tok_Div then Tok_Div
  else raise (InvalidInputException "InvalidInputException")


and parse_concat toks = 
  match toks with
  |[] -> raise (InvalidInputException "parse_concat")
  |h::t -> begin match lookahead (match_token toks h) with
    |None -> parse_unary toks
    |Some h2 -> if h2 <> Tok_Concat then parse_unary toks else
      begin match parse_unary toks with
      |(a1, a2) -> let rem_toks = (match_token a1 Tok_Concat) in begin match parse_concat rem_toks with
        |(a3, a4) -> (a3, Binop(Concat, a2, a4))
      end
    end
  end


and parse_unary toks = 
  match toks with
  |[] -> raise (InvalidInputException "parse_unary")
  |h::t -> if h = Tok_Not then let rem_toks = (match_token toks h) in parse_unary rem_toks
    else parse_call toks

and parse_call toks = 
  match toks with
  |[] -> raise (InvalidInputException "parse_call")
  |h::t -> begin match lookahead (match_token toks h) with
    |None -> parse_pri toks
    |Some h2 -> begin match h2 with 

      |Tok_Int h3 -> begin match parse_pri toks with
      |(a1, a2) -> begin match parse_pri a1 with
        |(a3, a4) -> (a3, FunctionCall(a2, a4))
      end
    end
      |Tok_Bool h3 -> begin match parse_pri toks with
      |(a1, a2) -> begin match parse_pri a1 with
        |(a3, a4) -> (a3, FunctionCall(a2, a4))
      end
    end
      |Tok_String h3 -> begin match parse_pri toks with
      |(a1, a2) -> begin match parse_pri a1 with
        |(a3, a4) -> (a3, FunctionCall(a2, a4))
      end
    end
      |Tok_ID h3 -> begin match parse_pri toks with
      |(a1, a2) -> begin match parse_pri a1 with
        |(a3, a4) -> (a3, FunctionCall(a2, a4))
      end
    end
      |Tok_LParen -> begin match parse_pri toks with
      |(a1, a2) -> begin match parse_pri a1 with
        |(a3, a4) -> (a3, FunctionCall(a2, a4))
      end
    end
      |_ -> parse_pri toks
   end 
 end
 (* parse_pri toks*)

and parse_pri toks = 
  match toks with 
  |[] -> raise (InvalidInputException "parse_pri: Empty List")
  |h::t -> begin match h with
    |Tok_Int h2 -> ((match_token toks (Tok_Int h2)), Value(Int h2))
    |Tok_Bool h2 -> ((match_token toks (Tok_Bool h2)), Value(Bool h2))
    |Tok_String h2 -> ((match_token toks (Tok_String h2)), Value(String h2))
    |Tok_ID h2 -> ((match_token toks (Tok_ID h2)), (ID h2))
    |Tok_LParen -> let rem_toks = match_token toks Tok_LParen in begin match parse_expr rem_toks with
      |(a1, a2) -> let rem_toks2 = match_token a1 Tok_RParen in (rem_toks2, a2)
      end
    |_ -> raise (InvalidInputException "parse_pri: Invalid Input")
  end

let rec list_nosemi lst =
  match lst with
  |[] -> []
  |[x] -> []
  |h::t -> h::(list_nosemi t)


let rec parse_mutop toks =
  match lookahead toks with
  |None -> raise (InvalidInputException "InvalidInputException")
  |Some h -> begin match h with
    |Tok_DoubleSemi -> ([], NoOp)
    |Tok_Def -> let rem_toks = match_token toks Tok_Def
      in begin match lookahead rem_toks with
      |None -> raise (InvalidInputException "InvalidInputException")
      |Some h -> let rem_toks2 = match_token rem_toks h in let rem_toks3 = match_token rem_toks2 Tok_Equal in
        match parse_expr (list_nosemi rem_toks3) with
        |(a1, a2) -> (a1, Expr(a2))
      end
    |_ -> match parse_expr (list_nosemi toks) with
      |(a1, a2) -> (a1, Expr(a2))
  end

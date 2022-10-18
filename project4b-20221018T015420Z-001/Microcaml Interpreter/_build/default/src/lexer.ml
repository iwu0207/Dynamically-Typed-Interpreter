open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)


let re_let = Str.regexp "let"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_equal = Str.regexp "="
let re_nequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not"
let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"
let re_add = Str.regexp "\\+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "\\*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_def = Str.regexp "def"
let re_in = Str.regexp "in"
let re_rec = Str.regexp "rec"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"
let re_doublesemi = Str.regexp ";;"
let re_str = Str.regexp "\"[^\n]*\""
let re_int1 = Str.regexp "[0-9]+"
let re_int2 = Str.regexp "(-[0-9]+)"
let re_bool1 = Str.regexp "true"
let re_bool2 = Str.regexp "false"
let re_id = Str.regexp "[a-zA-z][a-zA-Z0-9]*"
let re_quotes = Str.regexp "[a-zA-Z0-9]*\"[a-zA-Z0-9]*"

exception InvalidInputException of string



let tokenize input =
 let rec tok pos s =
   if pos >= String.length s then
     []
   else
     if (Str.string_match re_let s pos) then
       (Tok_Let)::(tok (pos+3) s)
     else if (Str.string_match re_int1 s pos) then
       let token = Str.matched_string s in
       (Tok_Int (int_of_string token))::(tok (pos+(String.length token)) s)
     else if (Str.string_match re_int2 s pos) then
       let token = Str.matched_string s in
       (Tok_Int (int_of_string (String.sub token 1 ((String.length token)-2))))::(tok (pos+(String.length token)) s)
     else if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tok (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tok (pos+1) s)
     else if (Str.string_match re_equal s pos) then
       Tok_Equal::(tok (pos+1) s)
     else if (Str.string_match re_nequal s pos) then
       Tok_NotEqual::(tok (pos+2) s)
     else if (Str.string_match re_arrow s pos) then
       Tok_Arrow::(tok (pos+2) s)
     else if (Str.string_match re_greaterequal s pos) then
       Tok_GreaterEqual::(tok (pos+2) s)
     else if (Str.string_match re_lessequal s pos) then
       Tok_LessEqual::(tok (pos+2) s)
     else if (Str.string_match re_greater s pos) then
       Tok_Greater::(tok (pos+1) s)
     else if (Str.string_match re_less s pos) then
       Tok_Less::(tok (pos+1) s)
     else if (Str.string_match re_or s pos) then
       Tok_Or::(tok (pos+2) s)
     else if (Str.string_match re_and s pos) then
       Tok_And::(tok (pos+3) s)
     else if (Str.string_match re_not s pos) then
       Tok_Not::(tok (pos+3) s)
     else if (Str.string_match re_if s pos) then
       Tok_If::(tok (pos+2) s)
     else if (Str.string_match re_then s pos) then
       Tok_Then::(tok (pos+4) s)
     else if (Str.string_match re_else s pos) then
       Tok_Else::(tok (pos+4) s)
     else if (Str.string_match re_add s pos) then
       Tok_Add::(tok (pos+1) s)
     else if (Str.string_match re_sub s pos) then
       Tok_Sub::(tok (pos+1) s)
     else if (Str.string_match re_mult s pos) then
       Tok_Mult::(tok (pos+1) s)
     else if (Str.string_match re_div s pos) then
       Tok_Div::(tok (pos+1) s)
     else if (Str.string_match re_concat s pos) then
       Tok_Concat::(tok (pos+1) s)
     else if (Str.string_match re_def s pos) then
       Tok_Def::(tok (pos+3) s)
     else if (Str.string_match re_in s pos) then
       Tok_In::(tok (pos+2) s)
     else if (Str.string_match re_rec s pos) then
       Tok_Rec::(tok (pos+3) s)
     else if (Str.string_match re_fun s pos) then
       Tok_Fun::(tok (pos+3) s)
     else if (Str.string_match re_doublesemi s pos) then
       Tok_DoubleSemi::(tok (pos+2) s)
     else if (Str.string_match re_str s pos) then
       let token = Str.matched_string s in let token2 = (String.sub token 1 ((String.length token)-2)) in
         if (Str.string_match re_quotes token2 0) then
           raise (InvalidInputException "InvalidInputException")
         else
           (Tok_String token2)::(tok (pos + (String.length token)) s)
     else if (Str.string_match re_bool1 s pos) then
       let token = Str.matched_string s in
       (Tok_Bool (bool_of_string token))::(tok (pos + (String.length token)) s)
     else if (Str.string_match re_bool2 s pos) then
       let token = Str.matched_string s in
       (Tok_Bool (bool_of_string token))::(tok (pos + (String.length token)) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       (Tok_ID (token))::(tok (pos + (String.length token)) s)
     else
       tok (pos+1) s
 in
 tok 0 input

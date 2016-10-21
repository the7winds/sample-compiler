open Ostap
open Matcher

module Expr =
  struct

    type t =
    | Const of int
    | Var   of string
    | Binop of string * t * t

    ostap (
      parse: ori;

      ori:
        l:andi suf:(("!!") andi)* {
          List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | andi;

      andi:
        l:cmp suf:(("&&") cmp)* {
          List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | cmp;

      cmp:
        l:addi suf:(("<=" | "<" | "==" | "!=" | ">=" | ">") addi)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | addi;

      addi:
        l:mulli suf:(("+" | "-") mulli)* {
          List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | mulli;

      mulli:
        l:primary suf:(("*" | "/" | "%") primary)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | primary;

      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var   x}
      | -"(" parse -")"
    )

  end

module Stmt =
  struct

    type t =
    | Skip
    | Read   of string
    | Write  of Expr.t
    | Assign of string * Expr.t
    | Seq    of t * t
    | IfElse of Expr.t * t * t
    | While  of Expr.t * t

    ostap (
      parse: s:simple d:(-";" parse)? {
        match d with None -> s | Some d -> Seq (s, d)
      };

      simple:
        x:IDENT ":=" e:!(Expr.parse)     {Assign (x, e)}
      | %"read"  "(" x:IDENT ")"         {Read x}
      | %"write" "(" e:!(Expr.parse) ")" {Write e}
      | %"skip"                          {Skip}
      | %"while" e:!(Expr.parse) %"do"
                 s:parse
        %"od"                            {While (e, s)}
      | %"if"    e:!(Expr.parse) %"then"
                 s:parse
            suf:( %"elif" !(Expr.parse)
                  %"then" parse )*
        es:(%"else" parse)?
	%"fi"
        {IfElse (e, s, List.fold_right
                           (fun (e, t) r -> IfElse (e, t, r)) 
                           suf
                           (match es with
                            | Some e -> e
                            | _ -> Skip))}
      | %"repeat" s:parse
        %"until" e:!(Expr.parse)         {Seq (s, While (Binop ("==", Const 0, e), s))}
      | %"for" s1:parse "," e:!(Expr.parse) "," s2:parse
        %"do"
               s:parse
        %"od"
                                         {Seq (s1, While (e, Seq(s, s2)))}
    )

  end

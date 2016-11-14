open Ostap
open Matcher

module Expr =
  struct

    type t =
    | Const of int
    | Var   of string
    | Binop of string * t * t
    | Call  of string * t list

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

      args:
        e:(parse)? suf:(-"," parse)* {
            match e with
            | Some w -> w::suf
            | _      -> []
        };

      primary:
        n:DECIMAL {Const n}
      | x:IDENT a:(-"(" args -")")?  {
          match a with
          | Some t -> Call (x, t)
          | _      -> Var   x
        }
      | -"(" parse -")"
    )

  end

module Stmt =
  struct

    type t =
    | Skip
    | Assign of string * Expr.t
    | Seq    of t * t
    | IfElse of Expr.t * t * t
    | While  of Expr.t * t
    | FunDcl of string * string list * t
    | Return of Expr.t
    | ExprSt of Expr.t

    ostap (
      parse: f:(func)* m:main {
        f@[FunDcl ("main", [], Seq (m, Return (Const 0)))]
      };

      main: s:simple d:(-";" main)? {
        match d with None -> s | Some d -> Seq (s, d)
      };

      args:
        e:(IDENT)? suf:(-"," IDENT)* {
            match e with
            | Some w -> w::suf
            | _      -> []
        };

      func:
        %"fun" f:IDENT "(" a:args ")"
        %"begin"
               s:main
        %"end"                           {FunDcl (f, a, s)};

      simple:
        x:IDENT ":=" e:!(Expr.parse)     {Assign (x, e)}
      | %"skip"                          {Skip}
      | %"while" e:!(Expr.parse) %"do"
                 s:main
        %"od"                            {While (e, s)}
      | %"if"    e:!(Expr.parse) %"then"
                 s:main
            suf:( %"elif" !(Expr.parse)
                  %"then" main )*
            es:(%"else" main)?
        %"fi"
        {
            IfElse (e, s, List.fold_right
                           (fun (e, t) r -> IfElse (e, t, r))
                           suf
                           (match es with
                            | Some e -> e
                            | _ -> Skip))
        }
      | %"repeat" s:main
        %"until" e:!(Expr.parse)         {Seq (s, While (Binop ("==", Const 0, e), s))}
      | %"for" s1:main "," e:!(Expr.parse) "," s2:main
        %"do"
               s:main
        %"od"                            {Seq (s1, While (e, Seq(s, s2)))}
      | %"return" e:!(Expr.parse)        {Return e}
      | e:!(Expr.parse)                  {ExprSt e}

    )

  end

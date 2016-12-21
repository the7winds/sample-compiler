open Ostap
open Matcher

open Builtin
module BV = Builtin.Value

module Expr =
  struct

    type t =
    | Const of BV.t
    | Var   of string
    | Binop of string * t * t
    | Call  of string * t list
    | Access of string * t list

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

      str:
        s:STRING {
            let b = Bytes.of_string s in
            let l = Bytes.length b in
            let b' = Bytes.sub b 1 (l-2) in
            BV.String b'
        };

      dec:
        n:DECIMAL {BV.Int n};

      seqInt:
        e:(dec)? suf:(-"," dec)* {
            match e with
            | Some w -> w::suf
            | _      -> []
        };

      intArr:
        "[" a:seqInt "]" {
            BV.Array (Array.of_list a)
        };

      boxed:
        "{" e:(intArr|str|boxed)? suf:(-"," (intArr|str|boxed))* "}" {
            let l = match e with
                    | Some w -> w::suf
                    | _      -> []
            in BV.Array (Array.of_list l)
        };

      idx: -"[" parse -"]";

      primary:
        n:dec               {Const n}
      | s:str               {Const s}
      | a:(intArr|boxed)    {Const a}
      | x:IDENT a:(-"(" args -")")? i:(idx)* {
          match a with
          | Some t -> Call (String.concat "" ["_"; x], t)
          | _      ->
                match i with
                | [] -> Var x
                | _  -> Access (x, i)
        }
      | c:CHAR {
            Const (BV.Int (Char.code c))
        }
      | -"(" parse -")"
    )

  end

module Stmt =
  struct

    type t =
    | Skip
    | Assign of Expr.t * Expr.t
    | Seq    of t * t
    | IfElse of Expr.t * t * t
    | While  of Expr.t * t
    | FunDcl of string * string list * t
    | Return of Expr.t
    | ExprSt of Expr.t
    | GDecl  of string

    ostap (
      parse: g:(gvar)* f:(func)* m:main {
        g@f@[FunDcl ("main", [], Seq (m, Return (Const (BV.Int 0))))]
      };

      gvar:
        %"global" x:IDENT {GDecl x};

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
        %"end"                           {FunDcl ((String.concat "" ["_"; f]), a, s)};

      simple:
        x:!(Expr.parse) ":=" e:!(Expr.parse) {Assign (x, e)}
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
        %"until" e:!(Expr.parse)         {Seq (s, While (Binop ("==", Const (BV.Int 0), e), s))}
      | %"for" s1:main "," e:!(Expr.parse) "," s2:main
        %"do"
               s:main
        %"od"                            {Seq (s1, While (e, Seq(s, s2)))}
      | %"return" e:!(Expr.parse)        {Return e}
      | e:!(Expr.parse)                  {ExprSt e}

    )

  end

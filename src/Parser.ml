open Ostap
open Matcher
open Expr

ostap (
    expr: prior5;

    prior5tail:
      tail:("||" r:prior4)*
        { (List.map (fun (op, r) -> ("||", r)) tail) };

    prior5:
      a:prior4 suf:prior5tail
        { List.fold_left (fun l (op, r) -> BinOp (op, l, r)) a suf };

    prior4tail:
      tail:("&&" r:prior3)*
        { (List.map (fun (op, r) -> ("&&", r)) tail) };

    prior4:
      a:prior3 suf:prior4tail
        { List.fold_left (fun l (op, r) -> BinOp (op, l, r)) a suf };

    eq:
      l:prior2 "==" r:prior2 { BinOp ("==", l, r) };
    nq:
      l:prior2 "!=" r:prior2 { BinOp ("!=", l, r) };
    lq:
      l:prior2 "<=" r:prior2 { BinOp ("<=", l, r) };
    gq:
      l:prior2 ">=" r:prior2 { BinOp (">=", l, r) };
    lt:
      l:prior2 "<"  r:prior2 { BinOp ("<", l, r) };
    gt:
      l:prior2 ">"  r:prior2 { BinOp (">", l, r) };
    cmp: eq | nq | lq | gq | lt | gt;

    prior3:
      cmp
    | prior2
    | -"(" a:prior5 -")" { a };

    addTail:
      "+" a:prior1 tail:("+" r:prior1)*
          { ("+", a)::(List.map (fun (op, r) -> ("+", r)) tail) };
    subTail:
      "-" a:prior1 tail:("-" r:prior1)*
          { ("-", a)::(List.map (fun (op, r) -> ("-", r)) tail) };
    addOrSubTail: addTail | subTail;
    prior2tail:
      tail:(l:addOrSubTail)* { List.concat tail };

    prior2:
      l:prior1 suf:prior2tail
           { List.fold_left (fun l (op, r) -> BinOp (op, l, r)) l suf };

    mulTail:
      "*" a:primary tail:("*" r:primary)*
           { ("*", a)::(List.map (fun (op, r) -> ("*", r)) tail) };
    divTail:
      "/" a:primary tail:("/" r:primary)*
           { ("/", a)::(List.map (fun (op, r) -> ("/", r)) tail) };
    modTail:
      "%" a:primary tail:("/" r:primary)*
           { ("%", a)::(List.map (fun (op, r) -> ("%", r)) tail) };
    mulOrDivTail: mulTail | divTail | modTail;
    prior1tail:
      tail:(l:mulOrDivTail)* { List.concat tail };

    prior1:
      l:primary suf:prior1tail
            { List.fold_left (fun l (op, r) -> BinOp (op, l, r)) l suf };

    primary:
      c:DECIMAL { Const c }
    | x:IDENT   { Var   x }
    | -"(" prior2 -")"
)

ostap (
    stmt:
      s1:simple ";" s2:stmt       { Seq    (s1, s2) }
    | simple;

    simple:
      %"read"  "(" name:IDENT ")"            { Read name          }
    | %"write" "(" e:expr     ")"            { Write e            }
    | %"skip"                                { Skip               }
    | x:IDENT ":=" e:expr                    { Assign (x , e)     }
    | %"if"    "(" e:expr     ")" "{" s1:stmt "}"
       "else"                     "{" s2:stmt "}"
                                             { IfElse (e, s1, s2) }
    | %"if"    "(" e:expr     ")" "{" s:stmt  "}"
                                             { If (e, s)          }
    | %"while" "(" e:expr     ")" "{" s:stmt  "}"
                                             { While (e, s)       }
)

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident ["read"; "write"; "skip"] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (stmt -EOF))

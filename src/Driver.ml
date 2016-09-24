open Expr
open Parser

(*
read (x);
read (y);
z := x * x;
write (z+y)
*)

(*
  let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)

let ( !! )       = (!)
let ( !  ) x     = Var x
let ( $  ) n     = Const n
let ( +  ) e1 e2 = BinOp ("+", e1, e2)
let ( *  ) e1 e2 = BinOp ("*", e1, e2)
let ( /  ) e1 e2 = BinOp ("/", e1, e2)
let ( %  ) e1 e2 = BinOp ("%", e1, e2)
let ( && ) e1 e2 = BinOp ("&&", e1, e2)
let ( >  ) e1 e2 = BinOp (">", e1, e2)
let ( == ) e1 e2 = BinOp ("==", e1, e2)

let skip     = Skip
let (:=) x e = Assign (x, e)
let read x   = Read x
let write x  = Write x
let (|>) l r = Seq (l, r)


let p =
  read "x" |>
  read "y" |>
  write (!"z")


(*
read (x);
read (y);
z := x * x;
write (z+y)


let p =
  read "x" |>
  read "y" |>
  ("z" := !"x" * !"x") |>
  write (!"z" + !"y")

(*
let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r

let run input p =
  srun input (compile_stmt p)

let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)
*)

let main =
  try
    let filename = Sys.argv.(1) in
    match Parser.parse filename with
    | `Ok stmt -> ignore @@ Expr.build stmt (Filename.chop_suffix filename ".expr")
    | `Fail er -> Printf.eprintf "%s" er
  with Invalid_argument _ ->
    Printf.printf "Usage: rc.byte <name.expr>"

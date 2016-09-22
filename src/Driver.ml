open Expr

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
let ( +  ) e1 e2 = BinOp (Add, e1, e2)
let ( *  ) e1 e2 = BinOp (Mul, e1, e2)
let ( /  ) e1 e2 = BinOp (Div, e1, e2)
let ( %  ) e1 e2 = BinOp (Mod, e1, e2)
let ( && ) e1 e2 = BinOp (And, e1, e2)
let ( >  ) e1 e2 = BinOp (Gt, e1, e2)
let ( == ) e1 e2 = BinOp (Eq, e1, e2)

let skip     = Skip
let (:=) x e = Assign (x, e)
let read x   = Read x
let write x  = Write x
let (|>) l r = Seq (l, r)


let p =
  read "x" |>
  read "y" |>
  ("z" := (BinOp (And, (!"x" > !"y"), BinOp (Eq, (!"x" % !"y"), Const 0)))) |>
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
  write (!"z" + !"y")  |>
  read "z" |>
  write (!"z")

let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
                
let run input p =
  srun input (compile_stmt p)

let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)

let run' code = Printf.printf "%s" (x86toStr (x86compile (compile_stmt code)))

(*
let _ = 
    let p =
      read "x" |>
      read "y" |>
      ("z" := !"x" * !"x") |>
      write (!"z")  
    in
    run' p
*)

let _ = run' p

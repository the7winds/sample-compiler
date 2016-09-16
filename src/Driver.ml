open Expr

(*
read (x);
read (y);
z := x * x;
write (z+y)
*)

let p =
  Seq (
      Read "x",
      Seq (
          Read "y",
          Seq (
              Assign ("z", Div (Var "x", Var "y")),
              Seq (
                 Write (Var "z"),
                 Seq (
                    Assign ("z", Mod (Var "x", Var "y")),
                    Write (Var "z")
                 )
              )
          )
      )
    )

(*
  let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)

let ( !! )       = (!)
let ( !  ) x     = Var x
let ( $  ) n     = Const n
let ( +  ) e1 e2 = Add (e1, e2)
let ( *  ) e1 e2 = Mul (e1, e2)
let ( /  ) e1 e2 = Div (e1, e2)
let ( %  ) e1 e2 = Mod (e1, e2)

let skip     = Skip
let (:=) x e = Assign (x, e)
let read x   = Read x
let write x  = Write x
let (|>) l r = Seq (l, r)

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

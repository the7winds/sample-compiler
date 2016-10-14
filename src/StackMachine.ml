type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string
| S_LBL   of string
| S_JMP   of string
| S_CJMP  of string * string


module Interpreter =
  struct

    open Interpreter.Expr

    let run input code =
      let rec run' (state, stack, input, output) code total =
       match code with
       | []       -> output
       | i::code' ->
           let (context, code'') = 
              let rec getLblCode s code =
                  match code with
                  | []               -> failwith (Printf.sprintf "WHERE IS NO LABEL %s" s)
                  | (S_LBL x)::code' -> if x = s then code' else getLblCode s code'
                  | _::code'         -> getLblCode s code'
              in
              (match i with
              | S_READ ->
                let y::input' = input in
                ((state, y::stack, input', output), code')
              | S_WRITE ->
                let y::stack' = stack in
                ((state, stack', input, output @ [y]), code')
              | S_PUSH n ->
                ((state, n::stack, input, output), code')
              | S_LD x ->
                ((state, (List.assoc x state)::stack, input, output), code')
              | S_ST x ->
                let y::stack' = stack in
                (((x, y)::state, stack', input, output), code')
              | S_BINOP s ->
                let r::l::stack' = stack in
                ((state, (Interpreter.Expr.evalBinOp s l r)::stack', input, output), code')
              | S_LBL s -> ((state, stack, input, output), code')
              | S_JMP s ->
                ((state, stack, input, output), getLblCode s total)
              | S_CJMP (c, s) ->
                let a::stack' = stack in
                ((state, stack', input, output), 
                    if (match c with
                        | "NZ" -> a <> 0
                        | "Z"  -> a =  0 
                        | _    -> failwith "BAD CONDITION") then getLblCode s total 
                                                            else code')
              )
           in
           run' context code'' total
      in
      run' ([], [], input, []) code code

  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]

    let stmt s =
        let rec stmt' s lbl =
            let getLblName lbl = Printf.sprintf "CL%d" lbl in
            match s with
            | Skip          -> ([],                 lbl)
            | Assign (x, e) -> (expr e @ [S_ST x],  lbl)
            | Read    x     -> ([S_READ; S_ST x],   lbl)
            | Write   e     -> (expr e @ [S_WRITE], lbl)
            | Seq    (l, r) -> 
                let (code1, lbl1) = stmt' l lbl in
                let (code2, lbl2) = stmt' r lbl1 in
                    (code1 @ code2, lbl2)
            | If     (e, s) -> 
                let lblEnd = getLblName lbl in
                let (st, lbl') = stmt' s (lbl+1) in
                (expr e @ [S_CJMP ("Z", lblEnd)] @ st @ [S_LBL lblEnd], lbl')
            | IfElse (e, s1, s2) ->
                let lblElse = getLblName lbl in
                let (st1, lbl1) = stmt' s1 (lbl+1) in
                let (st2, lbl2) = stmt' s2 (lbl1) in
                (expr e @ [S_CJMP ("Z", lblElse)] @ st1 @ [S_LBL lblElse] @ st2, lbl2)
            | While (e, s) ->
                let lblCnd = getLblName lbl in
                let lblEnd = getLblName (lbl+1) in
                let (st, lbl') = stmt' s (lbl+2) in
                ([S_LBL lblCnd] @ expr e @ [S_CJMP ("Z", lblEnd)] @ st @ [S_JMP lblCnd] @ [S_LBL lblEnd], lbl')
        in
        let (code, _) = stmt' s 0 in code

  end

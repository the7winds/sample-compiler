open Builtin.Value
module BV = Builtin.Value

type i =
| S_POP
| S_SPOP
| S_PUSH  of BV.t
| S_SPUSH
| S_LD    of string
| S_ST    of string
| S_BINOP of string
| S_LBL   of string
| S_JMP   of string
| S_CJMP  of string * string
| S_CALL  of string
| S_END
| S_FUN   of string * (string list)
| S_RET
| S_RSAVE
| S_RRESTORE


module Interpreter =
  struct

    open Interpreter.Expr

    let run input code =
      let rec run' (state, stack, input, output) code total fun_list =
          (*Printf.printf "STATE SZ %d\n" (List.length state);*)
       match code with
       | []       -> (BV.Int 0, input, output)
       | i::code' ->
            if i = S_RET
            then let s::stack' = stack in (s, input, output)
            else
               let (context, code'') =
                  let rec getLblCode s code =
                      match code with
                      | []               -> failwith (Printf.sprintf "WHERE IS NO LABEL %s" s)
                      | (S_LBL x)::code'
                            when x = s   -> code
                      | a::code'         ->
                            getLblCode s code'
                  in
                  (match i with
                  | S_PUSH n ->
                    ((state, n::stack, input, output), code')
                  | S_SPUSH ->
                    ((state, stack, input, output), code')
                  | S_RRESTORE ->
                    ((state, stack, input, output), code')
                  | S_RSAVE ->
                    ((state, stack, input, output), code')
                  | S_POP ->
                    let s::stack' = stack in
                    ((state, stack', input, output), code')
                  | S_SPOP ->
                    let r::_::stack' = stack in
                    ((state, r::stack', input, output), code')
                  | S_LD x ->
                    (*Printf.printf "HERE! %s\n" x;*)
                    ((state, (List.assoc x state)::stack, input, output), code')
                  | S_ST x ->
                    let y::stack' = stack in
                    (((x, y)::state, stack', input, output), code')
                  | S_BINOP s ->
                    let r::l::stack' = stack in
                    ((state, (Interpreter.Expr.evalBinOp s l r)::stack', input, output), code')
                  | S_LBL s ->
                    (*Printf.printf "LBL %s\n" s; *)
                    ((state, stack, input, output), code')
                  | S_JMP s ->
                    (*Printf.printf "JMP %s\n" s; *)
                    ((state, stack, input, output), getLblCode s total)
                  | S_CJMP (c, s) ->
                    (*Printf.printf "CJMP %s\n" s; *)
                    let a::stack' = stack in
                    ((state, stack', input, output),
                        if (match c with
                            | "NZ" -> (BV.to_int a) <> 0
                            | "Z"  -> (BV.to_int a) =  0
                            | _    -> failwith "BAD CONDITION") then getLblCode s total
                                                                else code')
                  | S_FUN (s, a) ->
                    (*Printf.printf "ARGS: %s\n" (String.concat " " a);*)
                    let rec prepareState stack ar =
                        match ar with
                        | []     -> []
                        | a::ar' -> let x::stack' = stack in
                                    (a, x)::prepareState stack' ar'
                    in
                    ((prepareState stack a, stack, input, output), code')
                  | S_CALL s ->
                    (*Printf.printf "CALL %s\n" s;*)
                    try (
                        let ((S_FUN (f, _))::_ as fcode) = List.find (fun ((S_FUN (f, _)::_)) -> f = s) fun_list in
                        let (r, i, o) = run' ([], stack, input, output) fcode fcode fun_list in
                        ((state, r::stack, i, o), code')
                    ) with 
                      | Not_found -> 
                            match s with
                            | "read"  -> 
                                let (r, i, o) = Builtin.read input output in
                                ((state, r::stack, i, o), code')
                            | "write" ->
                                let x::_ = stack in
                                let (r, i, o) = Builtin.write x input output in
                                ((state, r::stack, i, o), code')
                            | "strmake" ->
                                let n::x::_ = stack in
                                ((state, (Builtin.strmake n x)::stack, input, output), code')
                            | "strset" ->
                                let s::i::c::_ = stack in
                                ((state, (Builtin.strset s i c)::stack, input, output), code')
                            | "strget" ->
                                let s::i::_ = stack in
                                ((state, (Builtin.strget s i)::stack, input, output), code')
                            | "strdup" ->
                                let s::_ = stack in
                                ((state, (Builtin.strdup s)::stack, input, output), code')
                            | "strcat" ->
                                let s1::s2::_ = stack in
                                ((state, (Builtin.strcat s1 s2)::stack, input, output), code')
                            | "strcmp" ->
                                let s1::s2::_ = stack in
                                ((state, (Builtin.strcmp s1 s2)::stack, input, output), code')
                            | "strlen" ->
                                let s::_ = stack in
                                ((state, (Builtin.strlen s)::stack, input, output), code')
                            | "strsub" ->
                                let s::i::l::_ = stack in
                                ((state, (Builtin.strsub s i l)::stack, input, output), code')
                  )
               in
               run' context code'' total fun_list
      in
      let main_code = List.hd @@ List.rev code in
      let (_, _, o) = run' ([], [], input, []) main_code main_code code in o

  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]
    | Call (f, a)     -> 
            (List.concat @@ List.rev @@ List.map (fun x -> (expr x) @ [S_SPUSH]) a) 
            @ [S_CALL f] 
            @ (List.map (fun x -> S_SPOP) a)


    let stmt s =
        let rec stmt' s lbl =
            let getLblName lbl = Printf.sprintf "CL%d" lbl in
            match s with
            | Skip          -> ([],                 lbl)
            | Assign (x, e) -> (expr e @ [S_ST x],  lbl)
            | Seq    (l, r) ->
                let (code1, lbl1) = stmt' l lbl in
                let (code2, lbl2) = stmt' r lbl1 in
                (code1 @ code2, lbl2)
            | IfElse (e, s1, s2) ->
                let lblElse = getLblName lbl in
                let lblEnd = getLblName (lbl+1) in
                let (st1, lbl1) = stmt' s1 (lbl+2) in
                let (st2, lbl2) = stmt' s2 lbl1 in
                (expr e @ [S_CJMP ("Z", lblElse)] @ st1 @ [S_JMP lblEnd; S_LBL lblElse] @ st2 @ [S_LBL lblEnd], lbl2)
            | While (e, s) ->
                let lblCnd = getLblName lbl in
                let lblEnd = getLblName (lbl+1) in
                let (st, lbl') = stmt' s (lbl+2) in
                ([S_LBL lblCnd] @ expr e @ [S_CJMP ("Z", lblEnd)] @ st @ [S_JMP lblCnd] @ [S_LBL lblEnd], lbl')
            | FunDcl (f, a, s) ->
                let (fc, lbl') = stmt' s lbl in
                ([S_FUN (f, a)] @ fc, lbl')
            | Return e ->
                (expr e @ [S_RET], lbl)
            | ExprSt e ->
                (expr e @ [S_POP], lbl)
        in
        let rec stmt'' s n =
            match s with
            | [] -> []
            | f::fs -> let (fc, n') = stmt' f n in
                       (fc::stmt'' fs n')
        in stmt'' s 0
        (*Printf.printf "%s\n" (String.concat "\n" @@ List.map
            (fun x -> match x with
                      | S_READ          -> "READ"
                      | S_WRITE         -> "WRITE"
                      | S_POP           -> "POP"
                      | S_PUSH x        -> Printf.sprintf "PUSH %d" x
                      | S_LD s          -> Printf.sprintf "LD %s" s
                      | S_ST s          -> Printf.sprintf "SST %s" s
                      | S_BINOP s       -> Printf.sprintf "BINOP %s" s
                      | S_LBL s         -> Printf.sprintf "LABEL %s" s
                      | S_JMP s         -> Printf.sprintf "JMP %s" s
                      | S_CJMP (c, s)   -> Printf.sprintf "CJMP %s %s" c s
                      | S_CALL s        -> Printf.sprintf "CALL %s" s
                      | S_END           -> "END"
                      | S_FUN (s, a)    -> Printf.sprintf "FUN: %s ARGS: %s" s (String.concat " " a)
                      | S_RET           -> "RET"
        ) code); *)

  end

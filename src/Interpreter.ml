exception RetVal of int * (int list) * (int list)

module Expr =
  struct

    open Language.Expr

    let evalBinOp o l r =
       match o with
       | "+" -> l + r
       | "-" -> l - r
       | "*" -> l * r
       | "/" -> l / r
       | "%" -> l mod r
       | _ ->
           let lb = l <> 0 in
           let rb = r <> 0 in
           let boolToInt b = if b then 1 else 0 in
           boolToInt (
               match o with
               | "!!" -> lb || rb
               | "&&" -> lb && rb
               | "!=" -> l <> r
               | "<=" -> l <= r
               | ">=" -> l >= r
               | "==" -> l =  r
               | "<"  -> l <  r
               | ">"  -> l >  r)

    let rec eval fun_list state eval_fun_call input output = function
    | Const  n -> (n, input, output)
    | Var    x -> (state x, input, output)
    | Binop (o, l, r) ->
        let (lv, input', output')   = eval fun_list state eval_fun_call input  output  l in
        let (rv, input'', output'') = eval fun_list state eval_fun_call input' output' r in
        ((evalBinOp o lv rv), input'', output'')
    | Call  (f, a) ->
        let rec getArgs a input output =
            match a with
            | []    -> ([], input, output)
            | x::xs -> let (v, i, o)   = eval fun_list state eval_fun_call input output x in
                       let (t, i', o') = getArgs xs i o in
                       (v::t, i', o')
        in
        let (args, input', output') = getArgs a input output in
        eval_fun_call fun_list f args input' output'
  end

module Stmt =
  struct

    open Builtin
    open Language.Stmt

    let rec eval' fun_list ((None, (state, input, output)) as c) stmt =
      (*Printf.printf "HELLO eval\n";*)
      let state' x = List.assoc x state in
      match stmt with
      | Skip          -> c
      | Seq    (l, r) -> (
            match eval' fun_list c l with
            | ((None, _) as c') -> eval' fun_list c' r
            | c' -> c'
        )
      | Assign (x, e) ->
            (* (*Printf.printf "HELLO assign %s\n" x; *)*)
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (* (*Printf.printf "%s = %d\n" x v; *)*)
            (None, ((x, v)::state, input', output'))
      | IfElse (e, s1, s2) ->
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            eval' fun_list (None, (state, input', output')) (if v <> 0 then s1 else s2)
      | While (e, s) -> (
            let rec evalWhile e s (None, (state, input, output) as c) =
                let state' x = List.assoc x state in
                let (cond, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
                match cond with
                | 0 -> (None, (state, input', output'))
                | _ -> let c' = eval' fun_list c s in
                       match c' with
                       | (Some v, _) -> c'
                       | (None, _)   -> evalWhile e s c'
            in
            evalWhile e s c
        )
      | FunDcl (f, a, s) ->
            (*Printf.printf "HELLO fun declare %s\n" f;*)
            c
      | Return (e)       ->
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (Some v, (state, input', output'))
           (* raise (RetVal (v, input', output')) *)
      | ExprSt e         ->
            (*Printf.printf "HELLO expr st\n";*)
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (None, (state, input', output'))

    and eval_fun_call fun_list f args input output =
      (*Printf.printf "HELLO fun call %s\n" f;*)
        try (
          let FunDcl (_, args', stmt) = List.find (fun (FunDcl (f', _, _)) -> f' = f) fun_list in
          let state = List.combine args' args in
          let (r, (_, i, o)) = eval' fun_list (None, (state, input, output)) stmt in
          let Some v = r in (v, i, o)
        ) with
          | Not_found ->
                match f with
                | "read"  ->
                    Builtin.read input output
                | "write" -> 
                    let x::_ = args in
                    Builtin.write x input output
    let eval input fun_list =
      let FunDcl (_, _, main_code) = List.hd @@ List.rev fun_list in
      let (Some 0, (_, _, output)) = eval' fun_list (None, ([], input, [])) main_code
      in output

  end

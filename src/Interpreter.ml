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

    let rec eval state funs eval_fun_call input output = function
    | Const  n -> (n, input, output)
    | Var    x -> (state x, input, output)
    | Binop (o, l, r) ->
        let (lv, input', output') = eval state funs eval_fun_call input output l in
        let (rv, input'', output'') = eval state funs eval_fun_call input' output' r in
        ((evalBinOp o lv rv), input'', output'')
    | Call  (f, a) ->
        let rec getArgs a input output =
            match a with
            | []    -> ([], input, output)
            | x::xs -> let (v, i, o) = eval state funs eval_fun_call input output x in
                       let (t, i', o') = getArgs xs i o in
                       (v::t, i', o')
        in
        let (args, input', output') = getArgs a input output in
        eval_fun_call funs f args input' output'
  end

module Stmt =
  struct

    open Language.Stmt

    let rec eval' ((state, funs, input, output) as c) stmt =
      (*Printf.printf "HELLO eval\n";*)
      let state' x = List.assoc x state in
      match stmt with
      | Skip          -> c
      | Seq    (l, r) -> eval' (eval' c l) r
      | Assign (x, e) ->
            (* (*Printf.printf "HELLO assign %s\n" x; *)*)
            let (v, input', output') = Expr.eval state' funs eval_fun_call input output e in
            (* (*Printf.printf "%s = %d\n" x v; *)*)
            ((x, v) :: state, funs, input', output')
      | Write   e     ->
            (*Printf.printf "HELLO write\n";*)
            let (v, input', output') = Expr.eval state' funs eval_fun_call input output e in
            (*Printf.printf "write = %d\n" v;*)
            (state, funs, input', output' @ [v])
      | Read    x     ->
            (*Printf.printf "HELLO read %s\n" x;*)
            let y::input' = input in
            ((x, y) :: state, funs, input', output)
      | IfElse (e, s1, s2) ->
            let (v, input', output') = Expr.eval state' funs eval_fun_call input output e in
              if v <> 0 then eval' (state, funs, input', output') s1
                        else eval' (state, funs, input', output') s2
      | While (e, s) ->
            let rec evalWhile e s ((state, funs, input, output) as c) =
                let state' x = List.assoc x state in
                let (v, input', output') = Expr.eval state' funs eval_fun_call input output e in
                if v = 0 then (state, funs, input', output')
                         else let c' = eval' c s in evalWhile e s c'
          in
          evalWhile e s c
      | FunDcl (f, a, s) ->
            (*Printf.printf "HELLO fun declare %s\n" f;*)
            (state, (f, (a, s))::funs, input, output)
      | Return (e)       ->
            let (v, input', output') = Expr.eval state' funs eval_fun_call input output e in
            (("?", v)::state, funs, input', output)
      | ExprSt e         ->
            (*Printf.printf "HELLO expr st\n";*)
            let (v, input', output') = Expr.eval state' funs eval_fun_call input output e in
            (state, funs, input', output')

    and eval_fun_call funs f args input output =
      (*Printf.printf "HELLO fun call %s\n" f;*)
      let (args', stmt) = List.assoc f funs in
      let state = List.combine args' args in
      let (state', funs', input', output') = eval' (state, funs, input, output) stmt in
      (List.assoc "?" state', input', output')

    let eval input stmt =
      let (_, _, _, result) = eval' ([], [], input, []) (Seq (stmt, (ExprSt (Call ("main", []))))) in
      result

  end

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
        try eval_fun_call fun_list f args input' output' with
        | RetVal (v, i, o) -> (v, i, o)
  end

module Stmt =
  struct

    open Language.Stmt

    let rec eval' fun_list ((state, input, output) as c) stmt =
      (*Printf.printf "HELLO eval\n";*)
      let state' x = List.assoc x state in
      match stmt with
      | Skip          -> c
      | Seq    (l, r) -> eval' fun_list (eval' fun_list c l) r
      | Assign (x, e) ->
            (* (*Printf.printf "HELLO assign %s\n" x; *)*)
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (* (*Printf.printf "%s = %d\n" x v; *)*)
            ((x, v) :: state, input', output')
      | Write   e     ->
            (*Printf.printf "HELLO write\n";*)
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (*Printf.printf "write = %d\n" v;*)
            (state, input', output' @ [v])
      | Read    x     ->
            (*Printf.printf "HELLO read %s\n" x;*)
            let y::input' = input in
            ((x, y) :: state, input', output)
      | IfElse (e, s1, s2) ->
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            eval' fun_list (state, input', output') (if v <> 0 then s1 else s2)
      | While (e, s) ->
            let rec evalWhile e s ((state, input, output) as c) =
                let state' x = List.assoc x state in
                let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
                if v = 0 then (state, input', output')
                         else let c' = eval' fun_list c s 
                              in evalWhile e s c'
          in
          evalWhile e s c
      | FunDcl (f, a, s) ->
            (*Printf.printf "HELLO fun declare %s\n" f;*)
            c
      | Return (e)       ->
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (*(("?", v)::state, input', output')*)
            raise (RetVal (v, input', output'))
      | ExprSt e         ->
            (*Printf.printf "HELLO expr st\n";*)
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (state, input', output')

    and eval_fun_call fun_list f args input output =
      (*Printf.printf "HELLO fun call %s\n" f;*)
      let FunDcl (_, args', stmt) = List.find (fun (FunDcl (f', _, _)) -> f' = f) fun_list in
      let state = List.combine args' args in
      let (state', input', output') = eval' fun_list (state, input, output) stmt in
      (List.assoc "?" state', input', output')

    let eval input fun_list =
      let FunDcl (_, _, main_code) = List.hd @@ List.rev fun_list in
      try (let (_, _, output) = eval' fun_list ([], input, []) main_code in output) with
      | RetVal (0, _, output) -> output

  end

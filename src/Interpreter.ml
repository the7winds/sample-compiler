exception RetVal of int * (int list) * (int list)

module Expr =
  struct

    open Language.Expr
    open Builtin.Value
    
    module BV = Builtin.Value

    let evalBinOp o lv rv =
       let l = BV.to_int lv in
       let r = BV.to_int rv in
       BV.of_int (
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
               | ">"  -> l >  r
           )

    )

    let rec eval fun_list state eval_fun_call input output = function 
    | Const  n -> (n, input, output)
    | Var    x -> (state x, input, output)
    | Binop (o, l, r) ->
        let (lv, input', output')   = eval fun_list state eval_fun_call input  output  l in
        let (rv, input'', output'') = eval fun_list state eval_fun_call input' output' r in
        (evalBinOp o lv rv, input'', output'')
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
    | Access (a, i) ->
        match i with
        | [] -> (state a, input, output)
        | _  ->
            let (i', input', output') = 
                List.fold_left (
                    fun (_i, _in, _out) x -> 
                        let (_i', _in', _out') = eval fun_list state eval_fun_call _in _out x
                        in (_i@[_i'], _in', _out')
                ) ([], input, output) i in
            (List.fold_left (
                fun ar x -> 
                    Array.get (BV.of_array ar) (BV.to_int x)
                )
             (state a) i', input', output')
  end

module Stmt =
  struct

    open Builtin
    open Builtin.Value
    open Language.Stmt

    module BV = Builtin.Value

    let rec eval' fun_list ((None, (state, input, output)) as c) stmt =
      (* Printf.eprintf "HELLO eval\n"; *)
      let state' x = List.assoc x state in
      match stmt with
      | Skip          -> c
      | Seq    (l, r) -> (
            match eval' fun_list c l with
            | ((None, _) as c') -> eval' fun_list c' r
            | c' -> c'
        )
      | Assign (x, e) -> (
            (*Printf.eprintf "HELLO assign %s\n" x;*)
                match x with
                | Language.Expr.Var x ->
                    let (v, input', output') =
                        Expr.eval fun_list state' eval_fun_call input output e in
                    (None, ((x, v)::state, input', output'))
                | Language.Expr.Access (x', idxs) ->
                    let (v, i, o) = Expr.eval fun_list state' eval_fun_call input output e in
                    let idx::ridxs = List.rev idxs in
                    let (vi, i', o') = Expr.eval fun_list state' eval_fun_call i o idx in
                    let (va, i'', o'') = Expr.eval fun_list state' eval_fun_call i' o' (Language.Expr.Access (x', (List.rev ridxs))) in
                    let _ = Array.set (BV.of_array va) (BV.to_int vi) v in
                    (None, (state, i'', o''))
                (*Printf.eprintf "%s = %d\n" x (BV.to_int v);*)
        )
      | IfElse (e, s1, s2) ->
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            eval' fun_list (None, (state, input', output')) (if (BV.to_int v) <> 0 then s1 else s2)
      | While (e, s) -> (
            let rec evalWhile e s (None, (state, input, output) as c) =
                let state' x = List.assoc x state in
                let (cond, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
                match (cond) with
                | (BV.Int 0) -> (None, (state, input', output'))
                | _          -> let c' = eval' fun_list c s in
                                match c' with
                                | (Some v, _) -> c'
                                | (None, _)   -> evalWhile e s c'
            in
            evalWhile e s c
        )
      | FunDcl (f, a, s) ->
            (*Printf.eprintf "HELLO fun declare %s\n" f;*)
            c
      | Return (e)       ->
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (Some v, (state, input', output'))
      | ExprSt e         ->
            (*Printf.eprintf "HELLO expr st\n";*)
            let (v, input', output') = Expr.eval fun_list state' eval_fun_call input output e in
            (None, (state, input', output'))

    and eval_fun_call fun_list f args input output =
        (*Printf.eprintf "HELLO fun call %s\n" f;*)
        try (
          let FunDcl (_, args', stmt) = List.find (fun (FunDcl (f', _, _)) -> f' = f) fun_list in
          let state = List.combine args' args in
          let (r, (_, i, o)) = eval' fun_list (None, (state, input, output)) stmt in
          let Some v = r in (v, i, o)
        ) with
          | Not_found ->
                match f with
                | "_read"  ->
                    Builtin.read input output
                | "_write" -> 
                    let x::_ = args in
                    Builtin.write x input output
                | "_strmake" ->
                    let n::x::_ = args in
                    (Builtin.strmake n x, input, output)
                | "_strset" ->
                    let s::i::c::_ = args in
                    (Builtin.strset s i c, input, output) 
                | "_strget" ->
                    let s::i::_ = args in
                    (Builtin.strget s i, input, output)
                | "_strdup" ->
                    let s::_ = args in
                    (Builtin.strdup s, input, output)
                | "_strcat" ->
                    let s1::s2::_ = args in
                    (Builtin.strcat s1 s2, input, output)
                | "_strcmp" ->
                    let s1::s2::_ = args in
                    (Builtin.strcmp s1 s2, input, output)
                | "_strlen" ->
                    let s::_ = args in
                    (Builtin.strlen s, input, output)
                | "_strsub" ->
                    let s::i::l::_ = args in
                    (Builtin.strsub s i l, input, output)
                | "_arrmake" ->
                    let n::v::_ = args in
                    (Builtin.arrmake n v, input, output)
                | "_Arrmake" ->
                    let n::v::_ = args in
                    (Builtin.arrmake n v, input, output)
                | "_arrlen" ->
                    let a::_ = args in
                    (Builtin.arrlen a, input, output)
    let eval input fun_list =
      let FunDcl (_, _, main_code) = List.hd @@ List.rev fun_list in
      let (Some (BV.Int 0), (_, _, output)) = eval' fun_list (None, ([], input, [])) main_code
      in output

  end

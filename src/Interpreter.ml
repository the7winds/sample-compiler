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

    let rec eval fun_list state eval_fun_call g input output = function 
    | Const  n -> (n, g, input, output)
    | Var    x ->
        let el = if (List.mem_assoc x g) then List.assoc x g else state x in
        (el, g, input, output)
    | Binop (o, l, r) ->
        let (lv, g', input', output')   = eval fun_list state eval_fun_call g input  output  l in
        let (rv, g'', input'', output'') = eval fun_list state eval_fun_call g' input' output' r in
        (evalBinOp o lv rv, g'', input'', output'')
    | Call  (f, a) ->
        let rec getArgs a g input output =
            match a with
            | []    -> ([], g, input, output)
            | x::xs -> let (v, g', i, o)   = eval fun_list state eval_fun_call g input output x in
                       let (t, g'', i', o') = getArgs xs g' i o in
                       (v::t, g'', i', o')
        in
        let (args, g', input', output') = getArgs a g input output in
        eval_fun_call fun_list f args g' input' output'
    | Access (a, i) ->
        let el = if (List.mem_assoc a g) then List.assoc a g else state a in
        match i with
        | [] -> (el, g, input, output)
        | _  ->
            let (i', g', input', output') = 
                List.fold_left (
                    fun (_i, _g, _in, _out) x -> 
                        let (_i', _g', _in', _out') = eval fun_list state eval_fun_call _g _in _out x
                        in (_i@[_i'], _g', _in', _out')
                ) ([], g, input, output) i in
            (List.fold_left (fun ar x -> Array.get (BV.of_array ar) (BV.to_int x)) el i', g', input', output')
  end

module Stmt =
  struct

    open Builtin
    open Builtin.Value
    open Language.Stmt

    module BV = Builtin.Value

    let rec eval' fun_list ((None, (g, state, input, output)) as c) stmt =
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
                    let (v, g', input', output') =
                        Expr.eval fun_list state' eval_fun_call g input output e in
                    if (List.mem_assoc x g') then
                        (None, ((x, v)::g', state, input', output'))
                    else
                        (None, (g', (x, v)::state, input', output'))
                | Language.Expr.Access (x', idxs) ->
                    let (v, g', i, o) = Expr.eval fun_list state' eval_fun_call g input output e in
                    let idx::ridxs = List.rev idxs in
                    let (vi, g'', i', o') = Expr.eval fun_list state' eval_fun_call g' i o idx in
                    let (va, g2', i'', o'') = Expr.eval fun_list state' eval_fun_call g'' i' o' (Language.Expr.Access (x', (List.rev ridxs))) in
                    let _ = Array.set (BV.of_array va) (BV.to_int vi) v in
                    (None, (g2', state, i'', o''))
                (*Printf.eprintf "%s = %d\n" x (BV.to_int v);*)
        )
      | IfElse (e, s1, s2) ->
            let (v, g', input', output') = Expr.eval fun_list state' eval_fun_call g input output e in
            eval' fun_list (None, (g', state, input', output')) (if (BV.to_int v) <> 0 then s1 else s2)
      | While (e, s) -> (
            let rec evalWhile e s (None, (g, state, input, output) as c) =
                let state' x = List.assoc x state in
                let (cond, g', input', output') = Expr.eval fun_list state' eval_fun_call g input output e in
                match (cond) with
                | (BV.Int 0) -> (None, (g', state, input', output'))
                | _          -> let c' = eval' fun_list c s in (* ?????? WHY c not (g', input', output') ????? *)
                                match c' with
                                | (Some v, _) -> c'
                                | (None,   _) -> evalWhile e s c'
            in
            evalWhile e s c
        )
      | FunDcl (f, a, s) ->
            (*Printf.eprintf "HELLO fun declare %s\n" f;*)
            c
      | Return e         ->
            let (v, g', input', output') = Expr.eval fun_list state' eval_fun_call g input output e in
            (Some v, (g', state, input', output'))
      | ExprSt e         ->
            (*Printf.eprintf "HELLO expr st\n";*)
            let (v, g', input', output') = Expr.eval fun_list state' eval_fun_call g input output e in
            (None, (g', state, input', output'))

    and eval_fun_call fun_list f args g input output =
        (*Printf.eprintf "HELLO fun call %s\n" f;*)
        try (
          let FunDcl (_, args', stmt) = List.find (fun (FunDcl (f', _, _)) -> f' = f) fun_list in
          let state = List.combine args' args in
          let (r, (g', _, i, o)) = eval' fun_list (None, (g, state, input, output)) stmt in
          let Some v = r in (v, g', i, o)
        ) with
          | Not_found ->
                match f with
                | "_read"  ->
                    let x::input' = input in
                    (x, g, input', output)
                | "_write" -> 
                    let x::_ = args in
                    (BV.Int 0, g, input, output@[x])
                | f' -> (
                  (
                    match f' with
                    | "_strmake" ->
                        let n::x::_ = args in
                        Builtin.strmake n x
                    | "_strset" ->
                        let s::i::c::_ = args in
                        Builtin.strset s i c
                    | "_strget" ->
                        let s::i::_ = args in
                        Builtin.strget s i
                    | "_strdup" ->
                        let s::_ = args in
                        Builtin.strdup s
                    | "_strcat" ->
                        let s1::s2::_ = args in
                        Builtin.strcat s1 s2
                    | "_strcmp" ->
                        let s1::s2::_ = args in
                        Builtin.strcmp s1 s2
                    | "_strlen" ->
                        let s::_ = args in
                        Builtin.strlen s
                    | "_strsub" ->
                        let s::i::l::_ = args in
                        Builtin.strsub s i l
                    | "_arrmake" ->
                        let n::v::_ = args in
                        Builtin.arrmake n v
                    | "_Arrmake" ->
                        let n::v::_ = args in
                        Builtin.arrmake n v
                    | "_arrlen" ->
                        let a::_ = args in
                        Builtin.arrlen a
                   )
                    , g, input, output)
    let eval input gdecls_funs_list =
      let (g', funs) = List.partition (fun x ->
                                        match x with
                                        | GDecl _ -> true
                                        | _       -> false) gdecls_funs_list
      in
      let g = List.map (fun (GDecl x) -> (x, BV.Int 0)) g' in
      let FunDcl (_, _, main_code) = List.hd @@ List.rev funs in
      let (Some (BV.Int 0), (_, _, _, output)) = eval' funs (None, (g, [], input, [])) main_code
      in output

  end

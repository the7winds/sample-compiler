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

    let rec eval state = function
    | Const  n -> n
    | Var    x -> state x
    | Binop  (o, l, r) ->
        let lv = eval state l in
        let rv = eval state r in
        evalBinOp o lv rv

  end

module Stmt =
  struct

    open Language.Stmt

    let eval input stmt =
      let rec eval' ((state, input, output) as c) stmt =
        let state' x = List.assoc x state in
        match stmt with
        | Skip          -> c
        | Seq    (l, r) -> eval' (eval' c l) r
        | Assign (x, e) -> ((x, Expr.eval state' e) :: state, input, output)
        | Write   e     -> (state, input, output @ [Expr.eval state' e])
        | Read    x     ->
            let y::input' = input in
            ((x, y) :: state, input', output)
        | IfElse (e, s1, s2) -> if Expr.eval state' e <> 0 then eval' c s1 
                                                           else eval' c s2
        | While (e, s) ->
            let rec evalWhile e s ((state, input, output) as c) = 
                let state' x = List.assoc x state in
                if (Expr.eval state' e = 0) then c
                                            else let c' = eval' c s in
                                                 evalWhile e s c'
            in
            evalWhile e s c
      in
      let (_, _, result) = eval' ([], input, []) stmt in
      result

  end

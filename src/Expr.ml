type expr =
  | Const of int
  | Var   of string
  | Add   of expr * expr
  | Sub   of expr * expr
  | Mul   of expr * expr
  | Div   of expr * expr
  | Mod   of expr * expr 
  | And   of expr * expr
  | Or    of expr * expr
  | Eq    of expr * expr
  | NEq   of expr * expr
  | Less  of expr * expr
  | Leq   of expr * expr
  | Grtr  of expr * expr
  | Geq   of expr * expr

let rec eval state expr =
  match expr with
  | Const  n     -> n
  | Var    x     -> state x
  | Add   (l, r) -> eval state l + eval state r
  | Mul   (l, r) -> eval state l * eval state r
  | Div   (l, r) -> eval state l / eval state r
  | Mod   (l, r) -> eval state l mod eval state r
  | And   (l, r) -> if (eval state l) *  (eval state r) > 0 then 1 else 0
  | Or    (l, r) -> if (eval state l) +  (eval state l) > 0 then 1 else 0
  | Eq    (l, r) -> if (eval state l) == (eval state r) then 1 else 0
  | NEq   (l, r) -> if (eval state l) != (eval state r) then 1 else 0
  | Less  (l, r) -> if (eval state l) <  (eval state r) then 1 else 0
  | Leq   (l, r) -> if (eval state l) <= (eval state r) then 1 else 0
  | Grtr  (l, r) -> if (eval state l) >  (eval state r) then 1 else 0
  | Geq   (l, r) -> if (eval state l) >= (eval state r) then 1 else 0

type stmt =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of stmt * stmt
                       
let run input stmt =
  let rec run' ((state, input, output) as c) stmt =
    let state' x = List.assoc x state in
    match stmt with
    | Skip          -> c
    | Seq    (l, r) -> run' (run' c l) r
    | Assign (x, e) -> ((x, eval state' e) :: state, input, output)
    | Write   e     -> (state, input, output @ [eval state' e])
    | Read    x     ->
       let y::input' = input in
       ((x, y) :: state, input', output)
  in
  let (_, _, result) = run' ([], input, []) stmt in
  result
    
type instr =
  | S_READ
  | S_WRITE
  | S_PUSH  of int
  | S_LD    of string
  | S_ST    of string
  | S_ADD
  | S_SUB
  | S_MUL
  | S_DIV
  | S_MOD
  | S_AND
  | S_OR
  | S_EQ
  | S_NEQ
  | S_LT
  | S_LQ
  | S_GT
  | S_GQ

let srun input code =
  let rec srun' (state, stack, input, output) code =
    match code with
    | []       -> output
    | i::code' ->
       srun'
         (match i with
          | S_READ ->
             let y::input' = input in
             (state, y::stack, input', output)
          | S_WRITE ->
             let y::stack' = stack in
             (state, stack', input, output @ [y])
          | S_PUSH n ->
             (state, n::stack, input, output)
          | S_LD x ->
             (state, (List.assoc x state)::stack, input, output)
          | S_ST x ->
             let y::stack' = stack in
             ((x, y)::state, stack', input, output)
          | S_ADD ->
             let y::x::stack' = stack in
             (state, (x+y)::stack', input, output)
          | S_SUB ->
             let y::x::stack' = stack in
             (state, (x-y)::stack', input, output)
          | S_MUL ->
             let y::x::stack' = stack in
             (state, (x*y)::stack', input, output)
          | S_DIV ->
             let y::x::stack' = stack in
             (state, (x/y)::stack', input, output)
          | S_MOD ->
             let y::x::stack' = stack in
             (state, (x mod y)::stack', input, output)
          | S_AND ->
             let y::x::stack' = stack in
             (state, (if x * y > 0 then 1 else 0)::stack', input, output)
          | S_OR  ->
             let y::x::stack' = stack in
             (state, (if x + y > 0 then 1 else 0)::stack', input, output)
          | S_EQ  ->
             let y::x::stack' = stack in
             (state, (if x == y then 1 else 0)::stack', input, output)
          | S_NEQ ->
             let y::x::stack' = stack in
             (state, (if x != y then 1 else 0)::stack', input, output)
          | S_GQ  ->
             let y::x::stack' = stack in
             (state, (if x >= y then 1 else 0)::stack', input, output)
          | S_LQ ->
             let y::x::stack' = stack in
             (state, (if x <= y then 1 else 0)::stack', input, output)
          | S_GT  ->
             let y::x::stack' = stack in
             (state, (if x > y then 1 else 0)::stack', input, output)
          | S_LT ->
             let y::x::stack' = stack in
             (state, (if x < y then 1 else 0)::stack', input, output)
         )
         code'
  in
  srun' ([], [], input, []) code

let rec compile_expr expr =
  match expr with
  | Var    x     -> [S_LD   x]
  | Const  n     -> [S_PUSH n]
  | Add   (l, r) -> compile_expr l @ compile_expr r @ [S_ADD]
  | Sub   (l, r) -> compile_expr l @ compile_expr r @ [S_SUB]
  | Mul   (l, r) -> compile_expr l @ compile_expr r @ [S_MUL]
  | Div   (l, r) -> compile_expr l @ compile_expr r @ [S_DIV]
  | Mod   (l, r) -> compile_expr l @ compile_expr r @ [S_MOD]
  | And   (l, r) -> compile_expr l @ compile_expr r @ [S_AND]
  | Or    (l, r) -> compile_expr l @ compile_expr r @ [S_OR]
  | Eq    (l, r) -> compile_expr l @ compile_expr r @ [S_EQ]
  | NEq   (l, r) -> compile_expr l @ compile_expr r @ [S_NEQ]
  | Less  (l, r) -> compile_expr l @ compile_expr r @ [S_LT]
  | Grtr  (l, r) -> compile_expr l @ compile_expr r @ [S_GT]
  | Leq   (l, r) -> compile_expr l @ compile_expr r @ [S_LQ]
  | Geq   (l, r) -> compile_expr l @ compile_expr r @ [S_GQ]
  

let rec compile_stmt stmt =
  match stmt with
  | Skip          -> []
  | Assign (x, e) -> compile_expr e @ [S_ST x]
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expr e @ [S_WRITE]
  | Seq    (l, r) -> compile_stmt l @ compile_stmt r

let x86regs = [|"%eax"; "%edx"; "%ebx"; "%ecx"; "%esi"; "%edi"|]
let num_of_regs = Array.length x86regs
let word_size = 4

type opnd = R of int | S of int | M of string | L of int

let allocate stack =
  match stack with
  | []                              -> R 3
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

type x86instr =
  | X86Add  of opnd * opnd
  | X86Sub  of opnd * opnd
  | X86Mul  of opnd * opnd
  | X86Mov  of opnd * opnd
  | X86Cmp  of opnd * opnd
  | X86And  of opnd * opnd
  | X86Or   of opnd * opnd
  | X86Div  of opnd
  | X86Push of opnd
  | X86Pop  of opnd
  | X86Mark of int
  | X86Jz   of int
  | X86Jnz  of int
  | X86Je   of int
  | X86Jne  of int
  | X86Jmp  of int
  | X86Call of string
  | X86Cltd
  | X86Ret

let x86compile code =
  let rec x86compile' mark stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (mark', stack', x86code) =
         match i with
         | S_READ   -> (mark, [R 3], [X86Call "read";
                                      X86Mov  (R 0, R 3)])
         | S_PUSH n ->
            let s = allocate stack in
            (mark, s::stack, [X86Push (L n)])
         | S_WRITE  -> (mark, [], [X86Push    (R 3);
                                   X86Call    "write";
                                   X86Pop     (R 3)])
         | S_ST x   -> 
            let (s', c') = (
            let a::stack' = stack in
            match a with
            | S _ -> (stack', [X86Pop (R 0);
                               X86Mov (R 0, M x)])
            | R t -> (stack', [X86Mov (R t, M x)]))
            in (mark, s', c')
         | S_LD x   -> 
            let (s', c') = (
            let a = allocate stack in
            match a with
            | S _ -> (a::stack, [X86Push (M x)])
            | R t -> (a::stack, [X86Mov  (M x, R t)]))
            in (mark, s', c')
         | S_ADD    -> 
            let (s', c') = (
            let a::b::stack' = stack in
              match a with
              | R x -> (b::stack', [X86Add (a, b)])
              | S x ->
                match b with
                | R y -> (b::stack', [X86Add (a, b)])
                | S y -> (b::stack', [X86Pop    (R 0);
                                      X86Pop    (R 1);
                                      X86Add    (R 1, R 0);
                                      X86Push   (R 0)]))
            in (mark, s', c')
         | S_SUB    -> 
            let (s', c') = (
            let a::b::stack' = stack in
              match a with
              | R x -> (b::stack', [X86Sub (a, b)])
              | S x ->
                match b with
                | R y -> (b::stack', [X86Sub (a, b)])
                | S y -> (b::stack', [X86Pop    (R 0);
                                      X86Pop    (R 1);
                                      X86Sub    (R 1, R 0);
                                      X86Push   (R 0)]))
            in (mark, s', c')
         | S_MUL    -> 
            let (s', c') = ( 
            let a::b::stack' = stack in
              match b with
              | R x -> (b::stack', [X86Mul (a, b)])
              | S x ->
                match a with
                | R y -> (b::stack', [X86Mul (a, b)])
                | S y -> (b::stack', [X86Pop    (R 0);
                                      X86Pop    (R 1);
                                      X86Mul    (R 1, R 0);
                                      X86Push   (R 0)]))
             in (mark, s', c')
         | S_DIV    -> 
            let (s', c') = (
            let a::b::stack' = stack in
              match b with
              | R x -> (b::stack', [X86Mov  (b, R 0);
                                    X86Cltd;
                                    X86Div  (a);
                                    X86Mov  (R 0, b)])
              | S x ->
                match a with
                | R y -> (b::stack', [X86Pop    (R 0);
                                      X86Cltd;
                                      X86Div    (a);
                                      X86Push   (R 0)])
                | S y -> (b::stack', [X86Pop    (R 2);
                                      X86Pop    (R 0);
                                      X86Cltd;
                                      X86Div    (R 2);
                                      X86Push   (R 0)]))
            in (mark, s', c')
         | S_MOD    -> 
            let (s', c') = (
            let a::b::stack' = stack in
              match b with
              | R x -> (b::stack', [X86Mov  (b, R 0);
                                    X86Cltd;
                                    X86Div  (a);
                                    X86Mov  (R 1, b)])
              | S x ->
                match a with
                | R y -> (b::stack', [X86Pop    (R 0);
                                      X86Cltd;
                                      X86Div    (a);
                                      X86Push   (R 1)])
                | S y -> (b::stack', [X86Pop    (R 2);
                                      X86Pop    (R 0);
                                      X86Cltd;
                                      X86Div    (R 2);
                                      X86Push   (R 1)]))
            in (mark, s', c')
         | S_AND     -> (
            let a::b::stack' = stack in
              match b with
              | R x -> (mark + 2, b::stack', [X86And    (a,     b);
                                              X86Jz     (mark + 1);
                                              X86Mov    (L 1,   b);
                                              X86Jmp    (mark + 2);
                                              X86Mark   (mark + 1);
                                              X86Mov    (L 0,   b);
                                              X86Mark   (mark + 2)])
              | S x ->
                match a with
                | R y -> (mark + 2, b::stack', [X86Pop    (R 0);
                                                X86And    (R 0,   b);
                                                X86Jz     (mark + 1);
                                                X86Mov    (L 1,   b);
                                                X86Jmp    (mark + 2);
                                                X86Mark   (mark + 1);
                                                X86Mov    (L 0,   b);
                                                X86Mark   (mark + 2)])
                | S y -> (mark + 2, b::stack', [X86Pop    (R 0);
                                                X86Pop    (R 1);
                                                X86And    (R 0, R 1);
                                                X86Jz     (mark + 1);
                                                X86Mov    (L 1, R 1);
                                                X86Jmp    (mark + 2);
                                                X86Mark   (mark + 1);
                                                X86Mov    (L 0, R 1);
                                                X86Mark   (mark + 2);
                                                X86Push      (R 1)]))
         | S_OR      -> (
            let a::b::stack' = stack in
              match b with
              | R x -> (mark + 2, b::stack', [X86Or     (a,     b);
                                              X86Jz     (mark + 1);
                                              X86Mov    (L 1,   b);
                                              X86Jmp    (mark + 2);
                                              X86Mark   (mark + 1);
                                              X86Mov    (L 0,   b);
                                              X86Mark   (mark + 2)])
              | S x ->
                match a with
                | R y -> (mark + 2, b::stack', [X86Pop    (R 0);
                                                X86Or     (R 0,   b);
                                                X86Jz     (mark + 1);
                                                X86Mov    (L 1,   b);
                                                X86Jmp    (mark + 2);
                                                X86Mark   (mark + 1);
                                                X86Mov    (L 0,   b);
                                                X86Mark   (mark + 2)])
                | S y -> (mark + 2, b::stack', [X86Pop    (R 0);
                                                X86Pop    (R 1);
                                                X86Or     (R 0, R 1);
                                                X86Jz     (mark + 1);
                                                X86Mov    (L 1, R 1);
                                                X86Jmp    (mark + 2);
                                                X86Mark   (mark + 1);
                                                X86Mov    (L 0, R 1);
                                                X86Mark   (mark + 2);
                                                X86Push   (R 1)]))
         | S_EQ      -> (
            let a::b::stack' = stack in
              match b with
              | R x -> (mark + 2, b::stack', [X86Cmp    (a,     b);
                                              X86Je     (mark + 1);
                                              X86Mov    (L 0,   b);
                                              X86Jmp    (mark + 2);
                                              X86Mark   (mark + 1);
                                              X86Mov    (L 1,   b);
                                              X86Mark   (mark + 2)])
              | S x ->
                match a with
                | R y -> (mark + 2, b::stack', [X86Pop    (R 0);
                                                X86Cmp    (R 0,   b);
                                                X86Je     (mark + 1);
                                                X86Mov    (L 0,   b);
                                                X86Jmp    (mark + 2);
                                                X86Mark   (mark + 1);
                                                X86Mov    (L 1,   b);
                                                X86Mark   (mark + 2)])
                | S y -> (mark + 2, b::stack', [X86Pop    (R 0);
                                                X86Pop    (R 1);
                                                X86Cmp    (R 0, R 1);
                                                X86Je     (mark + 1);
                                                X86Mov    (L 0, R 1);
                                                X86Jmp    (mark + 2);
                                                X86Mark   (mark + 1);
                                                X86Mov    (L 1, R 1);
                                                X86Mark   (mark + 2);
                                                X86Push   (R 1)]))
         | S_NEQ      -> (
            let a::b::stack' = stack in
              match b with
              | R x -> (mark + 2, b::stack', [X86Cmp    (a,     b);
                                              X86Jne    (mark + 1);
                                              X86Mov    (L 0,   b);
                                              X86Jmp    (mark + 2);
                                              X86Mark   (mark + 1);
                                              X86Mov    (L 1,   b);
                                              X86Mark   (mark + 2)])
              | S x ->
                match a with
                | R y -> (mark + 2, b::stack', [X86Pop    (R 0);
                                                X86Cmp    (R 0,   b);
                                                X86Jne    (mark + 1);
                                                X86Mov    (L 0,   b);
                                                X86Jmp    (mark + 2);
                                                X86Mark   (mark + 1);
                                                X86Mov    (L 1,   b);
                                                X86Mark   (mark + 2)])
                | S y -> (mark + 2, b::stack', [X86Pop    (R 0);
                                                X86Pop    (R 1);
                                                X86Cmp    (R 0, R 1);
                                                X86Jne    (mark + 1);
                                                X86Mov    (L 0, R 1);
                                                X86Jmp    (mark + 2);
                                                X86Mark   (mark + 1);
                                                X86Mov    (L 1, R 1);
                                                X86Mark   (mark + 2);
                                                X86Push   (R 1)]))
       in
       x86code @ (x86compile' mark' stack' code')
  in
  (x86compile' 0 [] code) @ [X86Mov (L 0, R 0); X86Ret]

let x86toStr code =
    let printOp op =
      match op with
      | R n -> x86regs.(n)
      | M x -> x
      | L n -> Printf.sprintf "%s%d" "$" n
    in
    let printCmd cmd =
      match cmd with
      | X86Add  (a, b) -> Printf.sprintf "add %s, %s"  (printOp a) (printOp b)
      | X86Mul  (a, b) -> Printf.sprintf "imul %s, %s" (printOp a) (printOp b)
      | X86Mov  (a, b) -> Printf.sprintf "mov %s, %s"  (printOp a) (printOp b)
      | X86Div  a      -> Printf.sprintf "div %s"      (printOp a)
      | X86Push a      -> Printf.sprintf "push %s"     (printOp a)
      | X86Pop  a      -> Printf.sprintf "pop %s"      (printOp a)
      | X86Call s      -> Printf.sprintf "call %s"       (s)
      | X86Cltd        -> "cltd"
      | X86Ret         -> "ret"
    in
    let getVars code =
      let toOp c =
        match c with
        | X86Add (a, b) -> [a; b]
        | X86Mul (a, b) -> [a; b]
        | X86Push a     -> [a]
        | X86Pop  a     -> [a]
        | X86Mov (a, b) -> [a; b]
        | _             -> []
      in
      let pr o =
        match o with
        | M x -> true
        | _   -> false
      in
      let toS (M s) = s
      in
      List.sort_uniq compare (List.map toS (List.filter pr (List.concat (List.map toOp code))))
    in
    let toVar v = Printf.sprintf ".comm %s %d" v word_size
    in
    let vars : string list = getVars code
    in
    String.concat "\n" ([".text"] @ (List.map toVar vars) @ [".global main"; "main:"] @ (List.map printCmd code))

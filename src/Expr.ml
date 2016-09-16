type expr =
  | Const of int
  | Var   of string
  | Add   of expr * expr
  | Sub   of expr * expr
  | Mul   of expr * expr
  | Div   of expr * expr
  | Mod   of expr * expr 

let rec eval state expr =
  match expr with
  | Const  n     -> n
  | Var    x     -> state x
  | Add   (l, r) -> eval state l + eval state r
  | Mul   (l, r) -> eval state l * eval state r
  | Div   (l, r) -> eval state l / eval state r
  | Mod   (l, r) -> eval state l mod eval state r

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
  | X86Div  of opnd
  | X86Push of opnd
  | X86Pop  of opnd
  | X86Cltd
  | X86Ret
  | X86Call of string

let x86compile code =
  let rec x86compile' stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (stack', x86code) =
         match i with
         | S_READ   -> ([R 3], [X86Call "read";
                                X86Mov (R 0, R 3)])
         | S_PUSH n ->
            let s = allocate stack in
            (s::stack, [X86Push (L n)])
         | S_WRITE  -> ([], [X86Push (R 3);
                             X86Call "write";
                             X86Pop  (R 3)])
         | S_ST x   -> (
            let a::stack' = stack in
            match a with
            | S _ -> (stack', [X86Pop (R 0);
                               X86Mov (R 0, M x)])
            | R t -> (stack', [X86Mov (R t, M x)]))
         | S_LD x   -> (
            let a = allocate stack in
            match a with
            | S _ -> (a::stack, [X86Push (M x)])
            | R t -> (a::stack, [X86Mov  (M x, R t)]))
         | S_ADD    -> (
            let a::b::stack' = stack in
              match a with
              | R x -> (b::stack', [X86Add (a, b)])
              | S x ->
                match b with
                | R y -> (b::stack', [X86Add (a, b)])
                | S y -> (b::stack', [X86Pop (R 0);
                                      X86Pop (R 1);
                                      X86Add (R 1, R 0);
                                      X86Push (R 0)]))

         | S_SUB    -> (
            let a::b::stack' = stack in
              match a with
              | R x -> (b::stack', [X86Sub (a, b)])
              | S x ->
                match b with
                | R y -> (b::stack', [X86Sub (a, b)])
                | S y -> (b::stack', [X86Pop (R 0);
                                      X86Pop (R 1);
                                      X86Sub (R 1, R 0);
                                      X86Push (R 0)]))

         | S_MUL    -> ( 
            let a::b::stack' = stack in
              match b with
              | R x -> (b::stack', [X86Mul (a, b)])
              | S x ->
                match a with
                | R y -> (b::stack', [X86Mul (a, b)])
                | S y -> (b::stack', [X86Pop (R 0);
                                      X86Pop (R 1);
                                      X86Mul (R 1, R 0);
                                      X86Push (R 0)]))
         | S_DIV    -> (
            let a::b::stack' = stack in
              match b with
              | R x -> (b::stack', [X86Mov (b, R 0);
                                    X86Cltd;
                                    X86Div (a);
                                    X86Mov (R 0, b)])
              | S x ->
                match a with
                | R y -> (b::stack', [X86Pop (R 0);
                                      X86Cltd;
                                      X86Div (a);
                                      X86Push (R 0)])
                | S y -> (b::stack', [X86Pop (R 2);
                                      X86Pop (R 0);
                                      X86Cltd;
                                      X86Div (R 2);
                                      X86Push (R 0)]))
         | S_MOD    -> (
            let a::b::stack' = stack in
              match b with
              | R x -> (b::stack', [X86Mov (b, R 0);
                                    X86Cltd;
                                    X86Div (a);
                                    X86Mov (R 1, b)])
              | S x ->
                match a with
                | R y -> (b::stack', [X86Pop (R 0);
                                      X86Cltd;
                                      X86Div (a);
                                      X86Push (R 1)])
                | S y -> (b::stack', [X86Pop (R 2);
                                      X86Pop (R 0);
                                      X86Cltd;
                                      X86Div (R 2);
                                      X86Push (R 1)]))
       in
       x86code @ (x86compile' stack' code')
  in
  (x86compile' [] code) @ [X86Mov (L 0, R 0); X86Ret]

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

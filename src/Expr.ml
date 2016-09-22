type op = 
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Gt
  | Gq
  | Lt
  | Lq
  | Eq
  | Nq
  | And
  | Or

type expr =
  | Const of int
  | Var   of string
  | BinOp of op * expr * expr

let rec eval state expr =
  match expr with
  | Const  n     -> n
  | Var    x     -> state x
  | BinOp (o, l, r) -> 
    let lv = eval state l in
    let rv = eval state r in
    match o with
    | Add -> lv + rv
    | Sub -> lv - rv
    | Mul -> lv * rv
    | Div -> lv / rv
    | Mod -> lv mod rv
    | And -> if lv != 0 && rv != 0 then 1 else 0
    | Or  -> if lv != 0 || rv != 0 then 1 else 0
    | Eq  -> if lv == rv then 1 else 0
    | Nq  -> if lv != rv then 1 else 0
    | Gq  -> if lv >= rv then 1 else 0
    | Lq  -> if lv <= rv then 1 else 0
    | Gt  -> if lv > rv then 1 else 0
    | Lt  -> if lv < rv then 1 else 0

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
  | S_NQ
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
          | S_NQ ->
             let y::x::stack' = stack in
             (state, (if x != y then 1 else 0)::stack', input, output)
          | S_GQ  ->
             let y::x::stack' = stack in
             (state, (if y >= x then 1 else 0)::stack', input, output)
          | S_LQ ->
             let y::x::stack' = stack in
             (state, (if y <= x then 1 else 0)::stack', input, output)
          | S_GT  ->
             let y::x::stack' = stack in
             (state, (if y > x then 1 else 0)::stack', input, output)
          | S_LT ->
             let y::x::stack' = stack in
             (state, (if y < x then 1 else 0)::stack', input, output)
         )
         code'
  in
  srun' ([], [], input, []) code

let rec compile_expr expr =
  match expr with
  | Var    x         -> [S_LD   x]
  | Const  n         -> [S_PUSH n]
  | BinOp  (o, l, r) ->
    let lc = compile_expr l in
    let rc = compile_expr r in
    match o with
    | Add -> lc @ rc @ [S_ADD]
    | Sub -> lc @ rc @ [S_SUB]
    | Mul -> lc @ rc @ [S_MUL]
    | Div -> lc @ rc @ [S_DIV]
    | Mod -> lc @ rc @ [S_MOD]
    | And -> lc @ rc @ [S_AND]
    | Or  -> lc @ rc @ [S_OR]
    | Eq  -> lc @ rc @ [S_EQ]
    | Nq  -> lc @ rc @ [S_NQ]
    | Lt  -> lc @ rc @ [S_LT]
    | Gt  -> lc @ rc @ [S_GT]
    | Lq  -> lc @ rc @ [S_LQ]
    | Gq  -> lc @ rc @ [S_GQ]
  

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

type opnd = 
  | R  of int        (* for register mem *)
  | S  of int        (* for stack mem    *)
  | M  of string     (* for static vars  *)
  | L  of int        (* for immidiate    *)
  | MK of string     (* for marks        *)
  | F  of string     (* for functions    *)

let allocate stack =
  match stack with
  | []                              -> R 3
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

type x86instr =
  | X86Add  
  | X86Sub  
  | X86Mul  
  | X86Mov  
  | X86Cmp  
  | X86And  
  | X86Or   
  | X86Div  
  | X86Push 
  | X86Pop  
  | X86Jz   
  | X86Jnz  
  | X86Je   
  | X86Jg   
  | X86Jge  
  | X86Jl   
  | X86Jle  
  | X86Jne  
  | X86Jmp  
  | X86Call 
  | X86Cltd
  | X86Ret

type x86stmt =
  | X86Mark     of opnd
  | X860RInstr  of x86instr
  | X86UnInstr  of x86instr * opnd
  | X86BinInstr of x86instr * opnd * opnd

let x86compile code =
  let rec x86compile' mark stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (mark', stack', x86code) =
         let getSimpleArithmCode op l r res = [
            X86BinInstr (X86Mov, l, R 0);
            X86BinInstr (X86Mov, r, R 1);
            X86BinInstr (op, R 0, R 1);
            X86BinInstr (X86Mov, R 1, res)]
         in
         let getDivisionCode l r = [
            X86BinInstr (X86Mov, l, R 0);
            X86BinInstr (X86Mov, r, R 2);
            X860RInstr  (X86Cltd);
            X86UnInstr  (X86Div, R 2)]
         in
         let getLogicOpCode op l r res mark = 
           let m1 = MK Printf.sprintf "MK%d" (mark + 1) in
           let m2 = MK Printf.sprintf "MK%d" (mark + 2) in
             (mark + 2, [X86BinInstr (X86Mov, l, R 0);
                         X86BinInstr (X86Mov, r, R 1);
                         X86BinInstr (op, R 0, R 1);
                         X86UnInstr  (X86Jz, m1);
                         X86BinInstr (X86Mov, L 1, R 2);
                         X86UnInstr  (X86Jmp, m2);
                         X86Mark     m1;
                         X86BinInstr (X86Mov, L 0, R 2);
                         X86Mark     m2;
                         X86BinInstr (X86Mov, R 2, res)])
         in
         let getCmpOpCode op l r res mark =
           let m1 = MK Printf.sprintf "MK%d" (mark + 1) in
           let m2 = MK Printf.sprintf "MK%d" (mark + 2) in
             (mark + 2, [X86BinInstr (X86Mov, l, R 0);
                         X86BinInstr (X86Mov, r, R 1);
                         X86BinInstr (X86Cmp, R 1, R 0);
                         X86UnInstr  (op, m1);
                         X86BinInstr (X86Mov, L 0, R 2);
                         X86UnInstr  (X86Jmp, m2);
                         X86Mark     m1;
                         X86BinInstr (X86Mov, L 1, R 2);
                         X86Mark     m2;
                         X86BinInstr (X86Mov, R 2, res)])
         in
         match i with
         | S_READ   -> (mark, [R 3], [X86UnInstr (X86Call, F "read");
                                      X86BinInstr (X86Mov, R 0, R 3)])
         | S_PUSH n ->
            let s = allocate stack in
            (mark, s::stack, [X86BinInstr (X86Mov, L n, s)])
         | S_WRITE  -> (mark, [], [X86UnInstr (X86Push, R 3);
                                   X86UnInstr (X86Call, F "write");
                                   X86UnInstr (X86Pop, R 3)])
         | S_ST x   ->
            let a::stack' = stack in
            (mark, stack',  [X86BinInstr (X86Mov, a, R 0);
                             X86BinInstr (X86Mov, R 0, M x)])
         | S_LD x   ->
            let a = allocate stack in
            (mark, a::stack, [X86BinInstr (X86Mov, M x, R 0);
                              X86BinInstr (X86Mov, R 0, a)])
         | S_ADD    ->
            let r::l::stack' = stack in
            (mark, l::stack', getSimpleArithmCode X86Add l r l)
         | S_SUB    -> 
            let r::l::stack' = stack in
            (mark, l::stack', getSimpleArithmCode X86Sub l r l)
         | S_MUL    -> 
            let r::l::stack' = stack in
            (mark, l::stack', getSimpleArithmCode X86Mul l r l)
         | S_DIV    -> 
            let r::l::stack' = stack in
            (mark, l::stack', getDivisionCode l r @ [X86BinInstr (X86Mov, R 0, l)])
         | S_MOD    -> 
            let r::l::stack' = stack in
            (mark, l::stack', getDivisionCode l r @ [X86BinInstr (X86Mov, R 1, l)])
         | S_AND     -> 
            let r::l::stack' = stack in
            let (mark', cmd) = getLogicOpCode X86And l r l mark in
            (mark', l::stack', cmd)
         | S_OR      -> 
            let r::l::stack' = stack in
            let (mark', cmd) = getLogicOpCode X86Or l r l mark in
            (mark', l::stack', cmd)
         | S_EQ      ->
            let r::l::stack' = stack in
            let (mark', cmd) = getCmpOpCode X86Je l r l mark in
            (mark', l::stack', cmd)
         | S_GT       -> 
            let r::l::stack' = stack in
            let (mark', cmd) = getCmpOpCode X86Jg l r l mark in
            (mark', l::stack', cmd)
         | S_GQ      -> 
            let r::l::stack' = stack in
            let (mark', cmd) = getCmpOpCode X86Jge l r l mark in
            (mark', l::stack', cmd)
         | S_LT      -> 
            let r::l::stack' = stack in
            let (mark', cmd) = getCmpOpCode X86Jl l r l mark in
            (mark', l::stack', cmd)
         | S_LQ      ->
            let r::l::stack' = stack in
            let (mark', cmd) = getCmpOpCode X86Jle l r l mark in
            (mark', l::stack', cmd)
       in
       x86code @ (x86compile' mark' stack' code')
  in
  (x86compile' 0 [] code) @ [X86BinInstr (X86Mov, L 0, R 0); X860RInstr X86Ret]

let x86toStr code =
    let printOpnd op =
      match op with
      | R n  -> x86regs.(n)
      | M x  -> x
      | L n  -> Printf.sprintf "%s%d" "$" n
      | MK s -> s
      | F s  -> s
      | S n  -> Printf.sprintf "%d(%%ebp)" (-word_size * n)
    in
    let printX86Instr i =
      match i with
      | X86Add  -> "add"
      | X86Sub  -> "sub"
      | X86Mul  -> "mul"
      | X86Mov  -> "mov"
      | X86Cmp  -> "cmp"
      | X86And  -> "and"
      | X86Or   -> "or"
      | X86Div  -> "div"
      | X86Push -> "push"
      | X86Pop  -> "pop"
      | X86Jz   -> "jz"
      | X86Jnz  -> "jnz"
      | X86Je   -> "je"
      | X86Jg   -> "jg"
      | X86Jge  -> "jge"
      | X86Jl   -> "jl"
      | X86Jle  -> "jle"
      | X86Jne  -> "jne"
      | X86Jmp  -> "jmp" 
      | X86Call -> "call"
      | X86Cltd -> "cltd"
      | X86Ret  -> "ret"
    in
    let printX86Stmt stmt =
      match stmt with
      | X86Mark s ->
        let os = printOpnd s in
        Printf.sprintf "%s:" os
      | X860RInstr i -> printX86Instr i
      | X86BinInstr (i, l, r) -> 
        let ls = printOpnd l in
        let rs = printOpnd r in
        let is = printX86Instr i in
        Printf.sprintf "%s %s, %s" is ls rs
      | X86UnInstr (i, o) ->
        let os = printOpnd o in
        let is = printX86Instr i in
        Printf.sprintf "%s %s" is os
    in
    let getVars code  =
      let toOpnd stmt =
        match stmt with
        | X86BinInstr (_, a, b) -> [a; b]
        | X86UnInstr  (_, a)    -> [a]
        | _                     -> []
      in
      let pr o =
        match o with
        | M x -> true
        | _   -> false
      in
      let toS (M s) = s
      in
      List.sort_uniq compare (List.map toS (List.filter pr (List.concat (List.map toOpnd code))))
    in
    let toVar v = Printf.sprintf ".comm %s %d" v word_size
    in
    let vars : string list = getVars code
    in
    String.concat "\n" ([".text"] @ (List.map toVar vars) @ [".global main"; "main:"] @ (List.map printX86Stmt code))

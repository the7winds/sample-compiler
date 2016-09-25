type expr =
  | Const of int
  | Var   of string
  | BinOp of string * expr * expr

let rec eval state expr =
  match expr with
  | Const  n     -> n
  | Var    x     -> state x
  | BinOp (o, l, r) ->
    let lv = eval state l in
    let rv = eval state r in
    match o with
    | "+"  -> lv + rv
    | "-"  -> lv - rv
    | "*"  -> lv * rv
    | "/"  -> lv / rv
    | "%"  -> lv mod rv
    | "&&" -> if lv != 0 && rv != 0 then 1 else 0
    | "||" -> if lv != 0 || rv != 0 then 1 else 0
    | "==" -> if lv == rv then 1 else 0
    | "!=" -> if lv != rv then 1 else 0
    | ">=" -> if lv >= rv then 1 else 0
    | "<=" -> if lv <= rv then 1 else 0
    | ">"  -> if lv > rv then 1 else 0
    | "<"  -> if lv < rv then 1 else 0

type stmt =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of stmt * stmt
  | If     of expr * stmt
  | IfElse of expr * stmt * stmt
  | While  of expr * stmt

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
    | If     (e, s)      ->
        if eval state' e > 0
            then run' c s
            else c
    | IfElse (e, s1, s2) ->
        if eval state' e > 0
            then run' c s1
            else run' c s2
    | While  (e, s) ->
        let rec whileDo e s ((state, input, output) as c) =
          let state' x = List.assoc x state in
            if eval state' e > 0 then
                let c' = run' c s in
                whileDo e s c'
            else c
       in
       whileDo e s c
  in
  let (_, _, result) = run' ([], input, []) stmt in
  result

type instr =
  | S_READ
  | S_WRITE
  | S_PUSH  of int
  | S_LD    of string
  | S_ST    of string
  | S_LBL   of string
  | S_JMP   of string
  | S_JZ    of string
  | S_JNZ   of string
  | S_TESTZ
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
  let rec srun' ((state, stack, input, output) as context) ((upperCode, lowerCode) as code) =
    match lowerCode with
    | []       -> output
    | i::code' ->
       let rec splitByLabel (l, r) s =
          match r with
          | []    -> (l, r)
          | a::rs ->
             match a with
             | S_LBL s -> (a::l, rs)
             | _       -> splitByLabel (a::l, rs) s
       in
       let goto (upperCode, lowerCode) s =
          let (l, r) = splitByLabel ([], lowerCode) s in
          match l with
          | (S_LBL s)::ls -> (l @ upperCode, r)
          | _ ->
            let (l', r') = splitByLabel ([], upperCode) s in
            (r', l' @ lowerCode)
       in
       match i with
       | S_JMP s -> srun' context (goto code s)
       | S_JZ  s ->
          let c::stack' = stack in
          srun' (state, stack', input, output) (if (c == 0) then goto code s else (i::upperCode, code'))
       | S_JNZ s ->
          let c::stack' = stack in
          srun' (state, stack', input, output) (if (c != 0) then goto code s else (i::upperCode, code'))
       | _ -> srun' (
              match i with
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
              | S_LBL s -> context
             )
             (i::upperCode, code')
  in
  srun' ([], [], input, []) ([], code)

let rec compile_expr expr =
  match expr with
  | Var    x         -> [S_LD   x]
  | Const  n         -> [S_PUSH n]
  | BinOp  (o, l, r) ->
    let lc = compile_expr l in
    let rc = compile_expr r in
    match o with
    | "+"  -> lc @ rc @ [S_ADD]
    | "-"  -> lc @ rc @ [S_SUB]
    | "*"  -> lc @ rc @ [S_MUL]
    | "/"  -> lc @ rc @ [S_DIV]
    | "%"  -> lc @ rc @ [S_MOD]
    | "&&" -> lc @ rc @ [S_AND]
    | "||" -> lc @ rc @ [S_OR]
    | "==" -> lc @ rc @ [S_EQ]
    | "!=" -> lc @ rc @ [S_NQ]
    | "<"  -> lc @ rc @ [S_LT]
    | ">"  -> lc @ rc @ [S_GT]
    | "<=" -> lc @ rc @ [S_LQ]
    | ">=" -> lc @ rc @ [S_GQ]


let compile_stmt stmt =
   let rec compile_stmt' stmt lbl =
      match stmt with
      | Skip          -> ([], lbl)
      | Assign (x, e) -> (compile_expr e @ [S_ST x], lbl)
      | Read    x     -> ([S_READ; S_ST x], lbl)
      | Write   e     -> (compile_expr e  @ [S_WRITE], lbl)
      | Seq    (l, r) ->
        let (cmdl, llbl) = compile_stmt' l lbl in
        let (cmdr, rlbl) = compile_stmt' r llbl in
        (cmdl @ cmdr, rlbl)
      | If (e, s) ->
        let ec = compile_expr e in
        let ls = Printf.sprintf "LS%d" lbl in
        let (sc, lbl') = compile_stmt' s (lbl + 1) in
        (ec @ [S_TESTZ; S_JZ ls] @ sc @ [S_LBL ls], lbl')
      | IfElse (e, s1, s2) ->
        let ec = compile_expr e in
        let ls = Printf.sprintf "LS%d" lbl in
        let (sc1, lbl1) = compile_stmt' s1 (lbl + 1) in
        let (sc2, lbl2) = compile_stmt' s2 lbl1 in
        let lf = Printf.sprintf "LS%d" lbl2 in
        (ec @ [S_TESTZ; S_JZ ls] @ sc1 @ [S_JMP lf] @ [S_LBL ls] @ sc2 @ [S_LBL lf], lbl2 + 1)
      | While (e, s) ->
        let ec  = compile_expr e in
        let ls1 = Printf.sprintf "LS%d" lbl in
        let ls2 = Printf.sprintf "LS%d" (lbl + 1) in
        let (sc, lbl') = compile_stmt' s (lbl + 2) in
        ([S_LBL ls1] @ ec @ [S_TESTZ; S_JZ ls2] @ sc @ [S_JMP ls1] @ [S_LBL ls2], lbl')
   in
   let (code, _) = compile_stmt' stmt 1 in code


let x86regs = [|"%eax"; "%edx"; "%ebx"; "%esp"; "%ebp"; "%ecx"; "%esi"; "%edi"|]
let num_of_regs = Array.length x86regs
let word_size = 4

type opnd =
  | R  of int        (* for register mem *)
  | S  of int        (* for stack mem    *)
  | M  of string     (* for static vars  *)
  | L  of int        (* for immidiate    *)
  | LB of string     (* for labels       *)
  | F  of string     (* for functions    *)

let stack_start_reg = R 5

let allocate stack =
  match stack with
  | []                              -> stack_start_reg
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

let eax = R 0
let ebx = R 2
let edx = R 1
let esp = R 3
let ebp = R 4

let x86add = "add"
let x86sub = "sub"
let x86mul = "imul"
let x86mov = "mov"
let x86cmp = "cmp"
let x86div = "div"
let x86and = "and"
let x86or  = "or"
let x86jmp = "jmp"
let x86jz  = "jz"
let x86jnz = "jnz"
let x86je  = "je"
let x86jne = "jne"
let x86jl  = "jl"
let x86jle = "jle"
let x86jg  = "jg"
let x86jge = "jge"
let x86ret  = "ret"
let x86call = "call"
let x86cltd = "cltd"
let x86push = "push"
let x86pop  = "pop"
let x86test = "test"

type x86stmt =
  | X86Label     of opnd
  | X860RInstr  of string
  | X86UnInstr  of string * opnd
  | X86BinInstr of string * opnd * opnd

let x86compile code =
  let rec x86compile' label stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (label', stack', x86code) =
         let getSimpleArithmCode op l r res = [
            X86BinInstr (x86mov, l, eax);
            X86BinInstr (x86mov, r, edx);
            X86BinInstr (op, eax, edx);
            X86BinInstr (x86mov, edx, res)]
         in
         let getDivisionCode l r = [
            X86BinInstr (x86mov, l, eax);
            X86BinInstr (x86mov, r, ebx);
            X860RInstr  (x86cltd);
            X86UnInstr  (x86div, ebx)]
         in
         let getLogicOpCode op l r res label =
           let m1 = LB Printf.sprintf "LC%d" (label + 1) in
           let m2 = LB Printf.sprintf "LC%d" (label + 2) in
             (label + 2, [X86BinInstr (x86mov, l, eax);
                         X86BinInstr (x86mov, r, edx);
                         X86BinInstr (op, eax, edx);
                         X86UnInstr  (x86jz, m1);
                         X86BinInstr (x86mov, L 1, ebx);
                         X86UnInstr  (x86jmp, m2);
                         X86Label     m1;
                         X86BinInstr (x86mov, L 0, ebx);
                         X86Label     m2;
                         X86BinInstr (x86mov, ebx, res)])
         in
         let getCmpOpCode op l r res label =
           let m1 = LB Printf.sprintf "LC%d" (label + 1) in
           let m2 = LB Printf.sprintf "LC%d" (label + 2) in
             (label + 2, [X86BinInstr (x86mov, l, eax);
                         X86BinInstr (x86mov, r, edx);
                         X86BinInstr (x86cmp, edx, eax);
                         X86UnInstr  (op, m1);
                         X86BinInstr (x86mov, L 0, ebx);
                         X86UnInstr  (x86jmp, m2);
                         X86Label     m1;
                         X86BinInstr (x86mov, L 1, ebx);
                         X86Label     m2;
                         X86BinInstr (x86mov, ebx, res)])
         in
         match i with
         | S_READ   -> (label, [stack_start_reg], 
                                        [X86UnInstr (x86call, F "read");
                                         X86BinInstr (x86mov, eax, stack_start_reg)])
         | S_PUSH n -> (
            let s = allocate stack in
            match s with
            | R _ -> (label, s::stack, [X86BinInstr (x86mov, L n, s)])
            | _   -> (label, s::stack, [X86BinInstr (x86mov, L n, eax);
                                        X86BinInstr (x86mov, eax, s)]))
         | S_WRITE  -> (label, [], [X86UnInstr (x86push, stack_start_reg);
                                   X86UnInstr (x86call, F "write");
                                   X86UnInstr (x86pop, stack_start_reg)])
         | S_ST x   ->
            let a::stack' = stack in
            (label, stack',  [X86BinInstr (x86mov, a, eax);
                             X86BinInstr (x86mov, eax, M x)])
         | S_LD x   ->
            let a = allocate stack in
            (label, a::stack, [X86BinInstr (x86mov, M x, eax);
                              X86BinInstr (x86mov, eax, a)])
         | S_TESTZ ->
            let z::stack' = stack in
            (label, stack', [X86BinInstr (x86test, z, z)])
         | S_ADD    ->
            let r::l::stack' = stack in
            (label, l::stack', getSimpleArithmCode x86add l r l)
         | S_SUB    ->
            let r::l::stack' = stack in
            (label, l::stack', getSimpleArithmCode x86sub r l l)
         | S_MUL    ->
            let r::l::stack' = stack in
            (label, l::stack', getSimpleArithmCode x86mul l r l)
         | S_DIV    ->
            let r::l::stack' = stack in
            (label, l::stack', getDivisionCode l r @ [X86BinInstr (x86mov, eax, l)])
         | S_MOD    ->
            let r::l::stack' = stack in
            (label, l::stack', getDivisionCode l r @ [X86BinInstr (x86mov, edx, l)])
         | S_AND     ->
            let r::l::stack' = stack in
            let (label', cmd) = getLogicOpCode x86and l r l label in
            (label', l::stack', cmd)
         | S_OR      ->
            let r::l::stack' = stack in
            let (label', cmd) = getLogicOpCode x86or l r l label in
            (label', l::stack', cmd)
         | S_EQ      ->
            let r::l::stack' = stack in
            let (label', cmd) = getCmpOpCode x86je l r l label in
            (label', l::stack', cmd)
         | S_GT       ->
            let r::l::stack' = stack in
            let (label', cmd) = getCmpOpCode x86jg l r l label in
            (label', l::stack', cmd)
         | S_GQ      ->
            let r::l::stack' = stack in
            let (label', cmd) = getCmpOpCode x86jge l r l label in
            (label', l::stack', cmd)
         | S_LT      ->
            let r::l::stack' = stack in
            let (label', cmd) = getCmpOpCode x86jl l r l label in
            (label', l::stack', cmd)
         | S_LQ      ->
            let r::l::stack' = stack in
            let (label', cmd) = getCmpOpCode x86jle l r l label in
            (label', l::stack', cmd)
         | S_LBL s   ->
            (label + 1, stack, [X86Label (LB s)])
         | S_JMP s   ->
            (label, stack, [X86UnInstr (x86jmp, LB s)])
         | S_JZ s    ->
            (label, stack, [X86UnInstr (x86jz, LB s)])
       in
       x86code @ (x86compile' label' stack' code')
  in
  [X86UnInstr (x86push, ebp); X86BinInstr (x86mov, esp, ebp)] 
  @ (x86compile' 0 [] code)
  @ [X86UnInstr (x86pop, ebp); X86BinInstr (x86mov, L 0, eax); X860RInstr x86ret]

let x86toStr code =
    let printOpnd op =
      match op with
      | R n  -> x86regs.(n)
      | M x  -> x
      | L n  -> Printf.sprintf "%s%d" "$" n
      | LB s -> s
      | F s  -> s
      | S n  -> Printf.sprintf "%d(%%ebp)" (-word_size * n)
    in
    let printX86Stmt stmt =
      match stmt with
      | X86Label s ->
        let os = printOpnd s in
        Printf.sprintf "%s:" os
      | X860RInstr is -> is
      | X86BinInstr (is, l, r) ->
        let ls = printOpnd l in
        let rs = printOpnd r in
        Printf.sprintf "%s %s, %s" is ls rs
      | X86UnInstr (is, o) ->
        let os = printOpnd o in
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
    String.concat "\n" ([".text"] @ (List.map toVar vars) @ [".global main"; "main:"] @ (List.map printX86Stmt code) @ ["\n"])

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (x86toStr @@ x86compile @@ compile_stmt @@ stmt);
  close_out outf;
  Sys.command (Printf.sprintf "gcc -m32 -o %s ../runtime/runtime.o %s.s" name name)

type opnd = R of int | S of int | M of string | L of int

let x86regs = [|
  "%eax";
  "%ebx";
  "%edx";
  "%ecx";
  "%esi";
  "%edi"
|]

let x86regsByte = [|
  "%al";
  "%bl";
  "%dl";
  "%cl";
  "%sl";
  "%dl"
|]

let num_of_regs = Array.length x86regs
let word_size = 4

let eax = R 0
let ebx = R 1
let ecx = R 3
let edx = R 2
let esi = R 4
let edi = R 5

type instr =
| X86Add   of opnd * opnd
| X86Mul   of opnd * opnd
| X86Mov   of opnd * opnd
| X86Sub   of opnd * opnd
| X86Cmp   of opnd * opnd
| X86Div   of opnd
| X86Push  of opnd
| X86Pop   of opnd
| X86SetL  of opnd
| X86SetG  of opnd
| X86SetLE of opnd
| X86SetGE of opnd
| X86SetE  of opnd
| X86SetNE of opnd
| X86SetNZ of opnd
| X86And   of opnd * opnd
| X86Or    of opnd * opnd
| X86Ret
| X86Cltd
| X86Lbl   of string
| X86Jmp   of string
| X86Jz    of string
| X86Call  of string

module S = Set.Make (String)

class x86env =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    allocated  = ref 0
    method allocate n = allocated := max (n+1) !allocated
    method allocated  = !allocated
  end

let stackStart = 3

let allocate env stack =
  match stack with
  | []                              -> R stackStart
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> env#allocate 0; S 0

module Show =
  struct

    let opnd = function
    | R i -> x86regs.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i

    let opndByte = function
    | R i -> x86regsByte.(i)
    | op  -> opnd op

    let instr = function
    | X86Add (s1, s2) -> Printf.sprintf "\taddl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Mul (s1, s2) -> Printf.sprintf "\timull\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Sub (s1, s2) -> Printf.sprintf "\tsubl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Cmp (s1, s2) -> Printf.sprintf "\tcmpl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Mov (s1, s2) -> Printf.sprintf "\tmovl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Div  s       -> Printf.sprintf "\tidivl\t%s"      (opnd s )
    | X86Push s       -> Printf.sprintf "\tpushl\t%s"      (opnd s )
    | X86Pop  s       -> Printf.sprintf "\tpopl\t%s"       (opnd s )
    | X86Cltd         -> Printf.sprintf "\tcltd"
    | X86SetL  s      -> Printf.sprintf "\tsetl\t%s"       (opndByte s)
    | X86SetG  s      -> Printf.sprintf "\tsetg\t%s"       (opndByte s)
    | X86SetLE s      -> Printf.sprintf "\tsetle\t%s"      (opndByte s)
    | X86SetGE s      -> Printf.sprintf "\tsetge\t%s"      (opndByte s)
    | X86SetE  s      -> Printf.sprintf "\tsete\t%s"       (opndByte s)
    | X86SetNE s      -> Printf.sprintf "\tsetne\t%s"      (opndByte s)
    | X86SetNZ s      -> Printf.sprintf "\tsetnz\t%s"      (opndByte s)
    | X86And (s1, s2) -> Printf.sprintf "\tandl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Or  (s1, s2) -> Printf.sprintf "\torl\t%s,\t%s"   (opnd s1) (opnd s2)
    | X86Ret          -> "\tret"
    | X86Lbl  s       -> Printf.sprintf "%s:"           s
    | X86Jmp  s       -> Printf.sprintf "\tjmp\t%s"     s
    | X86Jz   s       -> Printf.sprintf "\tjz\t%s"      s
    | X86Call p       -> Printf.sprintf "\tcall\t%s"    p

  end

module Compile =
  struct

    open StackMachine

    let stack_program env code =
      let rec compile stack code =
        match code with
        | []       -> []
        | i::code' ->
            let (stack', x86code) =
              match i with
              | S_READ   ->
                  let s = allocate env stack in
                  (s::stack, [X86Call "read"; X86Mov (eax, s)])
              | S_WRITE  ->
                  let s::stack' = stack in
                  (stack', [X86Mov (s, ebx); X86Push ebx; X86Call "write"; X86Pop ebx])
              | S_PUSH n ->
                  let s = allocate env stack in
                  (s::stack, [X86Mov (L n, s)])
              | S_LD x   ->
                  env#local x;
                  let s = allocate env stack in
                  (s::stack,
                  match s with
                  | R _ -> [X86Mov (M x, s)]
                  | _   -> [X86Mov (M x, eax); X86Mov (eax, s)])
              | S_ST x   ->
                  env#local x;
                  let s::stack' = stack in
                  (stack',
                  match s with
                  | R _ -> [X86Mov (s, M x)]
                  | _   -> [X86Mov (s, eax); X86Mov (eax, M x)])
              | S_BINOP op -> (*failwith "x86 binop"*)
                  let lreg = eax in
                  let rreg = ebx in
                  let moveToReg l r lreg rreg = [X86Mov (l, lreg); X86Mov (r, rreg)] in
                  let r::l::stack' = stack in
                  (l::stack', moveToReg l r lreg rreg @
                             match op with
                             | "+" -> [X86Add (rreg, lreg); X86Mov (lreg, l)]
                             | "-" -> [X86Sub (rreg, lreg); X86Mov (lreg, l)]
                             | "*" -> [X86Mul (rreg, lreg); X86Mov (lreg, l)]
                             | "/" -> [X86Cltd; X86Div rreg; X86Mov (eax, l)]
                             | "%" -> [X86Cltd; X86Div rreg; X86Mov (edx, l)]
                             | "<" -> [X86Cmp (rreg, lreg); X86Mov (L 0, edx); X86SetL edx; X86Mov (edx, l)]
                             | ">" -> [X86Cmp (rreg, lreg); X86Mov (L 0, edx); X86SetG edx; X86Mov (edx, l)]
                             | "<=" -> [X86Cmp (rreg, lreg); X86Mov (L 0, edx); X86SetLE edx; X86Mov (edx, l)]
                             | ">=" -> [X86Cmp (rreg, lreg); X86Mov (L 0, edx); X86SetGE edx; X86Mov (edx, l)]
                             | "==" -> [X86Cmp (rreg, lreg); X86Mov (L 0, edx); X86SetE edx; X86Mov (edx, l)]
                             | "!=" -> [X86Cmp (rreg, lreg); X86Mov (L 0, edx); X86SetNE edx; X86Mov (edx, l)]
                             | "&&" -> [X86Cmp (L 0, lreg); X86SetNE lreg;
                                        X86Cmp (L 0, rreg); X86SetNE edx;
                                        X86And (lreg, edx); X86And (L 1, edx);
                                        X86Mov (edx, l)]
                             | "!!" -> [X86Cmp (L 0, lreg); X86SetNE lreg;
                                        X86Cmp (L 0, rreg); X86SetNE edx;
                                        X86Or (lreg, edx); X86And (L 1, edx);
                                        X86Mov (edx, l)])
              | S_LBL s -> (stack, [X86Lbl s])
              | S_JMP s -> (stack, [X86Jmp s])
              | S_CJMP (c, s) ->
                    let a::stack' = stack in
                    (stack',
                     (match a with
                     | R _ -> [X86Cmp (L 0, a)]
                     | _   -> [X86Mov (a, eax); X86Cmp (L 0, eax)]) @
                     (match c with
                     | "Z" -> [X86Jz s]
                     | _   -> failwith "BAD CONDITION x86"))
            in
            x86code @ compile stack' code'
      in
      compile [] code

  end

let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ StackMachine.Compile.stmt stmt in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars;
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
      ),
      (fun () ->
         !"\tmovl\t%ebp,\t%esp";
         !"\tpopl\t%ebp"
      )
  in
  !"main:";
  prologue();
  List.iter (fun i -> !(Show.instr i)) code;
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  ignore (Sys.command (Printf.sprintf "gcc -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name))

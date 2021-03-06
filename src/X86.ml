open Builtin.Value
module BV = Builtin.Value

type opnd =
    R  of int
  | S  of int
  | M  of string
  | L  of BV.t
  | A  of opnd

let x86regs = [|
  "%eax";
  "%ebx";
  "%edx";
  "%ebp";
  "%esp";
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
let edx = R 2
let ebp = R 3
let esp = R 4
let ecx = R 5
let esi = R 6
let edi = R 7

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

module M = Map.Make (String)

class x86env =
  object(self)
    val    local_vars = ref M.empty
    val    local_cnt  = ref 0
    val    args       = ref M.empty
    val    args_cnt   = ref 1

    method local x    = if not (M.mem x !local_vars) && not (M.mem x !args)
                        then local_vars := M.add x (!local_cnt + 1) !local_vars;
                             local_cnt := !local_cnt + 1;
    method local_n    = !local_cnt

    method arg x      = if not (M.mem x !local_vars) && not (M.mem x !args)
                        then args := M.add x (!args_cnt + 1) !args;
                             args_cnt := !args_cnt + 1

    method get_shift x = word_size * (if M.mem x !args then M.find x !args
                                                       else -(M.find x !local_vars))

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end

let stackStart = 5

let allocate env stack =
  match stack with
  | []                              -> R stackStart
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> env#allocate 1; S 1

class x86data =
  object(self)
    val    obj_cnt = ref 0
    val    obj_idx = ref []

    method add_const x =
        match x with
        | String s -> obj_cnt := !obj_cnt+1;
                      obj_idx := (x, !obj_cnt)::(!obj_idx);
        | Array a  -> Array.iter (self#add_const) a;
                      obj_cnt := !obj_cnt+1;
                      obj_idx := (x, !obj_cnt)::(!obj_idx);
        | _ -> ()

    method get_index o = List.assoc o !obj_idx

    method iterate f = List.iter f !obj_idx
  end

module Show =
  struct

    let instr data env =
        let rec opnd = function
        | R i -> x86regs.(i)
        | S i -> Printf.sprintf "-%d(%%ebp)" ((env#local_n + i) * word_size)
        | M x -> Printf.sprintf "%d(%%ebp)" (env#get_shift x)
        | L i -> (
            match i with
            | BV.Int d -> Printf.sprintf "$%d" d
            | _        -> Printf.sprintf "$obj%d" (data#get_index i)
          )
        | A o -> Printf.sprintf "(%s)" (opnd o)
        in
        let opndByte = function
        | R i -> x86regsByte.(i)
        | op  -> opnd op
        in
        function
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

    let stack_program env data code =
      let rec compile stack code =
        match code with
        | []       -> []
        | i::code' ->
            let (stack', x86code) =
              match i with
              | S_PUSH n ->
                  data#add_const n;
                  let s = allocate env stack in
                  (s::stack, [X86Mov (L n, s)])
              | S_SPUSH ->
                  let s::stack' = stack in
                  (stack', match s with
                           | R _ -> [X86Push s]
                           | _   -> [X86Mov (s, eax); X86Push eax])
              | S_SPOP ->
                  (stack, [X86Pop eax])
              | S_POP ->
                  let a::stack' = stack in
                  (stack', [X86Mov (a, eax)])
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
              | S_BINOP op ->
                  let r::l::stack' = stack in
                  (l::stack',
                        let getBinop lreg =
                             match op with
                             | "+" -> [X86Add (r, lreg)]
                             | "-" -> [X86Sub (r, lreg)]
                             | "*" -> [X86Mul (r, lreg)]
                             | "/" -> [X86Mov (l, eax); X86Cltd; X86Div r; X86Mov (eax, lreg)]
                             | "%" -> [X86Mov (l, eax); X86Cltd; X86Div r; X86Mov (edx, lreg)]
                             | "<" -> [X86Cmp (r, lreg); X86Mov (L (BV.Int 0), edx); X86SetL edx; X86Mov (edx, lreg)]
                             | ">" -> [X86Cmp (r, lreg); X86Mov (L (BV.Int 0), edx); X86SetG edx; X86Mov (edx, lreg)]
                             | "<=" -> [X86Cmp (r, lreg); X86Mov (L (BV.Int 0), edx); X86SetLE edx; X86Mov (edx, lreg)]
                             | ">=" -> [X86Cmp (r, lreg); X86Mov (L (BV.Int 0), edx); X86SetGE edx; X86Mov (edx, lreg)]
                             | "==" -> [X86Cmp (r, lreg); X86Mov (L (BV.Int 0), edx); X86SetE edx; X86Mov (edx, lreg)]
                             | "!=" -> [X86Cmp (r, lreg); X86Mov (L (BV.Int 0), edx); X86SetNE edx; X86Mov (edx, lreg)]
                             | "&&" -> [X86Cmp  (L (BV.Int 0), lreg);
                                        X86SetNE eax;
                                        X86Cmp  (L (BV.Int 0), r);
                                        X86SetNE edx;
                                        X86And  (eax, edx);
                                        X86And  (L (BV.Int 1), edx);
                                        X86Mov  (edx, lreg)]
                             | "!!" -> [X86Cmp  (L (BV.Int 0), lreg);
                                        X86SetNE eax;
                                        X86Cmp  (L (BV.Int 0), r);
                                        X86SetNE edx;
                                        X86Or   (eax, edx);
                                        X86And  (L (BV.Int 1), edx);
                                        X86Mov  (edx, lreg)]
                        in match l with
                             | R _ -> getBinop l
                             | _   -> [X86Mov (l, ebx)] @ getBinop ebx @ [X86Mov (ebx, l)]
                   )
              | S_LBL s -> (stack, [X86Lbl s])
              | S_JMP s -> (stack, [X86Jmp s])
              | S_CJMP (c, s) -> (
                    let a::stack' = stack in
                    (stack',
                     (match a with
                     | R _ -> [X86Cmp (L (BV.Int 0), a)]
                     | _   -> [X86Mov (a, eax); X86Cmp (L (BV.Int 0), eax)]) @
                     (match c with
                     | "Z" -> [X86Jz s]
                     | _   -> failwith "BAD CONDITION x86"))
              )
              | S_RET   ->
                    let a::stack' = stack in
                    (stack', [X86Mov (a, eax);
                        (* it restores previous frame stack-registers *)
                              X86Pop edi;
                              X86Pop esi;
                              X86Pop ecx;
                        (* done *)
                              X86Mov (ebp, esp);
                              X86Pop ebp;
                              X86Ret])
              | S_FUN (f, a) ->
                      List.iter (fun x -> env#arg x) a;
                      (stack, [X86Lbl f;
                            (* it saves register stack *)
                               X86Push ecx;
                               X86Push esi;
                               X86Push edi])
              | S_CALL f ->
                    let a = allocate env stack in
                    (a::stack, [X86Call f; X86Mov (eax, a)])
              | S_ELEM ->
                    let i::a::stack' = stack in
                    (a::stack', [X86Mov (i, eax);
                                 X86Mul (L (BV.of_int 4), eax);
                                 X86Add (L (BV.of_int 4), eax);
                                 X86Add (a, eax);
                                 X86Mov (A eax, ebx);
                                 X86Mov (ebx, a)])
              | S_STA ->
                    let i::a::e::stack' = stack in
                    (stack', [X86Mov (i, eax);
                              X86Mul (L (BV.of_int 4), eax);
                              X86Add (L (BV.of_int 4), eax);
                              X86Add (a, eax);
                              X86Mov (e, ebx);
                              X86Mov (ebx, A eax)])
            in
            x86code @ compile stack' code'
      in
      compile [] code

  end

let compile stmt =
  let data = new x86data in
  let codes = List.map
                    (fun sp -> let env = new x86env in
                               let p = Compile.stack_program env data sp in
                               (data, env, p))
                    @@ StackMachine.Compile.stmt stmt in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  let datainfo data =
        !".data";
        data#iterate (fun (o, n) -> !(Printf.sprintf "obj%d:" n);
                                    match o with
                                    | BV.String s ->
                                        !(Printf.sprintf "\t.int %d" (Bytes.length s));
                                        !(Printf.sprintf "\t.ascii \"%s\"" s);
                                    | BV.Array a ->
                                        !(Printf.sprintf "\t.int %d" (Array.length a));
                                        Array.iter (
                                            fun x -> match x with
                                                     | BV.Int d -> !(Printf.sprintf "\t.int %d" d)
                                                     | _ -> !(Printf.sprintf "\t.int obj%d" (data#get_index x))
                                        ) a;
        )
  in
  datainfo data;
  !"\t.text";
  !"\t.globl\tmain";
  let prologue env =
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" ((env#local_n + env#allocated) * word_size))
  in

  List.iter (fun (data, env, code) ->
      !(Show.instr data env @@ List.hd code);
      prologue env;
      List.iter (fun i -> !(Show.instr data env i)) @@ List.tl code;
  ) codes;
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  match Sys.command (Printf.sprintf "gcc -m32 -g -o %s $RC_RUNTIME/runtime.o %s.s" name name) with
  | 0 -> ()
  | _ -> failwith "gcc failed with non-zero exit code"

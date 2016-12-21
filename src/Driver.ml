open Ostap
open Builtin.Value

module BV = Builtin.Value

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident ["skip"; "if"; "then"; "else"; "for"; "fi"; "do"; "od"] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.string s
       inherit Util.Lexers.char s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip.nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (!(Language.Stmt.parse) -EOF))

let main = ()
  try
    let mode, filename =
      match Sys.argv.(1) with
      | "-s" -> `SM , Sys.argv.(2)
      | "-o" -> `X86, Sys.argv.(2)
      | _    -> `Int, Sys.argv.(2)
    in
    match parse filename with
    | `Ok stmt -> 
        (match mode with
         | `X86 ->
             let basename = Filename.chop_suffix filename ".expr" in 
             X86.build stmt basename
         | _ ->
            let rec read acc =
            try
                let r = BV.Int (read_int ()) in
                Printf.printf "> ";
                read (acc @ [r]) 
                with End_of_file -> acc
	        in
            let input = read [] in
            let output =
                match mode with
                | `SM  -> StackMachine.Interpreter.run input (StackMachine.Compile.stmt stmt)
                | _    -> Interpreter.Stmt.eval input stmt
            in
            List.iter (fun i -> Printf.printf "%s\n" (BV.str i)) output
        )

    | `Fail er -> Printf.eprintf "%s\n" er
  with 
  | Invalid_argument _ ->
      Printf.printf "Usage: rc.byte <command> <name.expr>\n";
      Printf.printf "  <command> should be one of: -i, -s, -o\n"

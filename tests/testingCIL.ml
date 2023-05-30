open Sys


let _ =
  (* if Array.length Sys.argv != 3 then
    raise (UsageError "Arguments <IFCIL-input-file> and <NuSMV-output-file> are needed"); *)
  let in_file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_file in
  let ruleset = CILparser.main CILlexer.token lexbuf in
  CILenv.print (CILenv.initialrho (CILsyntax.removeIN ruleset))
  
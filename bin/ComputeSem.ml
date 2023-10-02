open SELinuxIFCIL.IFCILsyntax
let print_atort = function
  | Any -> print_string "any"
  | A_Type qn -> 
      print_string ("type: " ^ (String.concat "." qn))
  | A_Attr qn ->
      print_string ("attribute: " ^ (String.concat "." qn))

let _ =
  if Array.length Sys.argv != 2 then
    failwith "Arguments <CIL-input-file> is needed";
    let in_file = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel in_file in
    let config' = 
      (SELinuxIFCIL.IFCILparser.main SELinuxIFCIL.IFCILlexer.token lexbuf) in
    let config =
      SELinuxIFCIL.IFCILsyntax.removeIN config' in
    print_endline " --> closing configfile\n";
    close_in in_file;
    print_endline " --> computing semantics\n";
    try
      (
      let semantics = SELinuxIFCIL.IFCILsemanticsE.get_semantics (config)
      in let semAllows =
        List.map 
          (fun (a,b,c) -> (a,c))
        semantics.allows
      in 
      List.iter
        (fun (src, dst) -> 
          print_atort src; 
          print_string " - ";
          print_atort dst;
          print_string "\n"
          )
          semAllows
      )
    with 
      | Not_found -> (
          failwith "Not Found in computing semantics")
      | Failure s -> (
          failwith ("Failed computing semantics " ^ s))

  

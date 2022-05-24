open CILsyntax
open IFCILconfiguration
open IFL
open Sys
module SS = Set.Make (String)
module Dict = Map.Make (String)

exception UsageError of string

let temp = "NuSMVoutput"

let parse_response_line line mustfalse =
  try
    if String.sub line 0 16 = "-- specification" then
      try
        if
          (String.sub line (String.length line - 8) 8 = "is false" && mustfalse)
          || String.sub line (String.length line - 7) 7 = "is true"
             && not mustfalse
        then "-------- is satisfied!\n\n"
        else "-------- is violated!\n\n"
      with Invalid_argument s ->
        raise (Invalid_argument (s ^ " when parsing responses"))
    else ""
  with Invalid_argument s -> ""

let rec parse_response oc' requirements =
  if requirements <> [] then (
    try
      let mustfalse =
        not
          (String.sub (snd (List.hd requirements)) 0 10 = ".IFL-every" 
          || 
          String.sub (snd (List.hd requirements)) 0 12 = ".IFL-mustnot")
      in
      let line = input_line oc' in
      let result = parse_response_line line mustfalse in
      if result <> "" then (
        print_string
          ("++++ "
          ^ fst (List.hd requirements)
          ^ " "
          ^ (snd (List.hd requirements))
          ^ result);
        parse_response oc' (List.tl requirements))
      else parse_response oc' requirements
    with
    | End_of_file -> close_in oc'
    | e ->
        (* some unexpected exception occurs *)
        close_in_noerr oc';
        (* emergency closing *)
        raise e)

let rec parse_requirements_from_NuSMV file =
  try
    let line = input_line file in
    if (String.trim (String.sub line 0 4)) = "--" then 
      let openPar = String.index line '(' in
      let closePar = String.index line ')' in
      [
        (String.trim (String.sub line openPar (closePar - openPar + 1)),
        String.trim (String.sub line (closePar + 1) (String.length line - ((closePar + 1)))))
      ]
      @ parse_requirements_from_NuSMV file
    else 
      parse_requirements_from_NuSMV file
  with
  | End_of_file -> 
      []
  | _ -> parse_requirements_from_NuSMV file

let _ =
  if Array.length Sys.argv != 2 then
    raise (UsageError "Arguments <NuSMV-configuration-file> are needed");
  let in_file = open_in Sys.argv.(1) in
  let requirements = parse_requirements_from_NuSMV in_file in
  close_in_noerr in_file;
  Sys.command ("./NuSMV  -v 0 " ^ Sys.argv.(1) ^ " > " ^ temp ^ " 2>/dev/null");
  let tmp_file = open_in temp in
  parse_response tmp_file requirements;


    
  
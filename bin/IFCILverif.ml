open SELinuxIFCIL.IFCILsyntax
open SELinuxIFCIL.IFL
open SELinuxIFCIL.Utils
open SELinuxIFCIL.IFCILsemanticsE
open ReadNuSMV

module SS = Set.Make (String)
module Dict = Map.Make (String)

exception UsageError of string

let is_write_only os =
  List.find_opt
    (fun s -> s = os)
    [
      "audit_access";
      "audit_control";
      "audit_read";
      "audit_write";
      "check_context";
      "getattr";
      "getcap";
      "getopt";
      "getpgid";
      "getrlimit";
      "getsched";
      "getsession";
      "read";
      "read_policy";
      "receive";
      "recv";
      "recvfrom";
      "search";
      "unix_read";
      "watch";
      "watch_mount";
      "watch_reads";
      "watch_sb";
      "watch_with_perm";
      "view";
    ]
  = None

let is_read_only os =
  List.find_opt
    (fun s -> s = os)
    [
      "add_name";
      "append";
      "call";
      "checkpoint_restore";
      "chfn";
      "chown";
      "chsh";
      "create";
      "create_files_as";
      "destroy";
      "fsetid";
      "kill";
      "mount";
      "name_bind";
      "name_connect";
      "net_bind_service";
      "passwd";
      "relabelfrom";
      "relabelto";
      "remount";
      "remove_name";
      "rename";
      "reparent";
      "rmdir";
      "send";
      "sendto";
      "set_context_mgr";
      "setattr";
      "setbool";
      "setcap";
      "setcheckreqprot";
      "setcontext";
      "setcurrent";
      "setenforce";
      "setexec";
      "setfcap";
      "setfscreate";
      "setgid";
      "setkeycreate";
      "setopt";
      "setpcap";
      "setpgid";
      "setrlimit";
      "setsched";
      "setsecparam";
      "setsockcreate";
      "setuid";
      "shutdown";
      "tracepoint";
      "transfer";
      "transition";
      "unix_write";
      "write";
    ]
  = None

module SLS = Set.Make (StringList)

let name node = String.concat "#" (List.tl node)

let rec types_of attributeqn types ta attributetotypeset =
  let rec types_of_exp xattributetotypeset = function
    | E_NAME Any -> 
      (
        types,
        xattributetotypeset
      )
    | E_NAME A_Type n -> 
        let typesinside = SLS.of_list [n] in
        (
          typesinside,
          xattributetotypeset
        )
    | E_NAME A_Attr n ->
        let attributetotypeset' = types_of n types ta attributetotypeset
        in
        (try
          (
            (SLM.find n attributetotypeset'),
            attributetotypeset'
          )
        with Not_found -> failwith "not found in types_of_exp")
    | E_OR (aexp1, aexp2) ->
        let (typesin1, attributetotypeset1) = types_of_exp xattributetotypeset aexp1 in
        let (typesin2, attributetotypeset2) = types_of_exp attributetotypeset1 aexp2 in
        (
          SLS.union typesin1 typesin2,
          attributetotypeset2
        )
    | E_AND (aexp1, aexp2) ->
        let (typesin1, attributetotypeset1) = types_of_exp xattributetotypeset aexp1 in
        let (typesin2, attributetotypeset2) = types_of_exp attributetotypeset1 aexp2 in
        (
          SLS.inter typesin1 typesin2,
          attributetotypeset2
        )
    | E_XOR (aexp1, aexp2) ->
        let (typesin1, attributetotypeset1) = types_of_exp xattributetotypeset aexp1 in
        let (typesin2, attributetotypeset2) = types_of_exp attributetotypeset1 aexp2 in
        (
          SLS.diff 
            (SLS.union typesin1 typesin2)
            (SLS.inter typesin1 typesin2),
          attributetotypeset2
        )
    | E_NOT aexp1 ->
      let (typesin1, attributetotypeset1) = types_of_exp xattributetotypeset aexp1 in
      (
        SLS.diff types typesin1,
        attributetotypeset1
      )
  in
  if (SLM.find_opt attributeqn attributetotypeset) != None then attributetotypeset else
  let (typesin, attributetotypeset') =
    (match SLM.find_opt attributeqn ta with
    | None -> (SLS.empty, attributetotypeset)
    | Some x -> types_of_exp attributetotypeset x)
  in
    SLM.update
      attributeqn
      (function 
          None -> Some typesin
        | Some x -> Some (SLS.union x typesin))
      attributetotypeset'
  (* with Not_found -> failwith ("not found in types_of - " ^ (name attributeqn)) *)

let infloflow allows =
  List.fold_left
    (fun flows (aort1, ops, aort2) ->
      let opsdir = SS.filter (fun op -> not (is_read_only op)) ops
      and opsinv = SS.filter (fun op -> not (is_write_only op)) ops 
      in
      (aort1, opsdir, aort2) :: ((aort2, opsinv, aort1) :: flows)
      )
    []
    allows

let get_transitions infoflows types tamap =
  let typesset = SLS.of_list types
  (* and tamap = 
    List.fold_left
      (fun xtamap (qn, attrexE) ->
        SLM.update
          qn
          (function 
            | None -> Some attrexE
            | Some xex -> Some (E_OR (xex, attrexE))
          )
          xtamap
        )
      SLM.empty
      ta *)
  and add_flow xiffl tgn x =
    SLM.update
      tgn
      (function
          Some op_per_aort_lst -> Some (x :: op_per_aort_lst)
        | None -> Some [x]
      )
      xiffl
  in
  let attributetotypeset = ref SLM.empty in
  List.fold_left
    (fun xiffl (aort1, ops, aort2) ->
      match (aort1:torat_node) with
        | Any -> raise (UsageError "any is not permitted in allow statements")
        | A_Type tgn -> add_flow xiffl tgn (ops, aort2)
        | A_Attr agn -> 
            attributetotypeset := types_of agn typesset tamap !attributetotypeset;
            try (
              SLS.fold
                (fun t xiffl' -> add_flow xiffl' t (ops, aort2))
                (SLM.find agn !attributetotypeset)
                xiffl           
            ) with Not_found -> failwith "attribute not found in get_transitions"
      )
    SLM.empty
    infoflows

let attrexptoNuSMV ex =
  let rec attrexptoNuSMV' ex =
    match ex with
      | E_NAME Any -> "TRUE"
      | E_NAME A_Type n -> "state = " ^ name n
      | E_NAME A_Attr n -> name n
      | E_NOT ex' -> "! ( " ^ attrexptoNuSMV' ex' ^ ")"
      | E_AND (ex', ex'') ->
            "( " ^ attrexptoNuSMV' ex' ^ " & " ^ attrexptoNuSMV' ex'' ^ ")"
      | E_OR (ex', ex'') ->
            "( " ^ attrexptoNuSMV' ex' ^ " | " ^ attrexptoNuSMV' ex'' ^ ")"
      | E_XOR (ex', ex'') ->
            "( " ^ attrexptoNuSMV' ex' ^ " xor " ^ attrexptoNuSMV' ex'' ^ ")"
    in
    "(" ^ attrexptoNuSMV' ex ^ ") & ! (state = pozzo)"

let print_attributes ta attributes oc =
  List.iter 
  (fun att -> 
    match SLM.find_opt att ta with
    | Some ex ->
          let exstr = attrexptoNuSMV ex in
          Printf.fprintf oc "%s := %s;\n" (name att) exstr
    | None ->
          Printf.fprintf oc "%s := FALSE;\n" (name att))
  attributes


let print_types types oc =
  Printf.fprintf oc "\n\n     VAR\n\n         state : { pozzo, %s  };\n\n"
    (String.concat ",\n              "
       (List.map (fun t -> name t) types))

let print_operations ops oc =
  Printf.fprintf oc "\n\n     IVAR\n\n         operation : { %s  };\n"
    (String.concat ",\n              "
       (SS.elements ops))

let print_allows allows oc types ta =
  Printf.fprintf oc "\n\n    TRANS\n";
  let infoflows = infloflow allows in
  let transitions = get_transitions infoflows types ta
  in
  SLM.iter
    (
      fun tqn opsandtargetlst ->
        Printf.fprintf oc "(state = %s -> ( " (name tqn);
        List.iter 
          (fun (ops, toratn) ->
            match toratn with
            | Any -> failwith "any cannot be in allow statement"
            | A_Type t -> 
                SS.iter
                (
                  fun o ->
                  Printf.fprintf oc "(operation = %s & next(state = %s)) | " o (name t)
                )
                ops
            | A_Attr a -> 
                SS.iter
                (
                  fun o ->
                  Printf.fprintf oc "(operation = %s & next(%s)) | " o (name a)
                )
                ops
          )
          opsandtargetlst;
        Printf.fprintf oc " next(state = pozzo) )) &\n"
    )
    transitions;
  Printf.fprintf oc "(state = pozzo -> next(state = pozzo))\n\n"

let rec kindE_to_LTL k =
  let print_o o =
    let os = List.map (fun oi -> "operation = " ^ oi) o in
    if List.length os > 1 then "(" ^ String.concat " | " os ^ ")"
    else String.concat " | " os
  and print_type = function
    | Any -> "(! (state = pozzo))"
    | A_Type n -> "state = " ^ name n
    | A_Attr n -> name n
  in
  match k with
  | [] -> "TRUE"
  | [ (n1, ([ "any-mod" ], SHORTARROW), n2) ] ->
      print_type n1 ^ " &  X " ^ print_type n2
  | [ (n1, (o, SHORTARROW), n2) ] ->
      print_type n1 ^ " &  " ^ print_o o ^ " & X " ^ print_type n2
  | [ (n1, ([ "any-mod" ], LONGARROW), n2) ] ->
      print_type n1 ^ " &  X ( F " ^ print_type n2 ^ ")"
  | [ (n1, (o, LONGARROW), n2) ] ->
      print_type n1 ^ " &  " ^ print_o o ^ " & X (" ^ print_o o ^ " U "
      ^ print_type n2 ^ ")"
  | (n1, ([ "any-mod" ], SHORTARROW), n2) :: ks ->
      print_type n1 ^ " &  X (" ^ kindE_to_LTL ks ^ ")"
  | (n1, (o, SHORTARROW), n2) :: ks ->
      print_type n1 ^ " & " ^ print_o o ^ " &  X (" ^ kindE_to_LTL ks
      ^ ")"
  | (n1, ([ "any-mod" ], LONGARROW), n2) :: ks ->
      print_type n1 ^ " &  X ( F (" ^ kindE_to_LTL ks ^ "))"
  | (n1, (o, LONGARROW), n2) :: ks ->
      print_type n1 ^ " & " ^ print_o o ^ " &  X ( " ^ print_o o ^ " U ("
      ^ kindE_to_LTL ks ^ "))"

let iflE_to_LTL rqr =
  match rqr with
  | E_MUST k | E_MUSTNOT k -> "!(" ^ kindE_to_LTL k ^ ")"
  | E_EVERYMUST (k1, k2) ->
      "(!(" ^ kindE_to_LTL k1 ^ ") | ("
      ^ kindE_to_LTL k2
      ^ "))"

let print_requirements iflreqs oc =
  List.iter
    (fun (name, rqr) ->
      Printf.fprintf oc "--  (%s) %s \n" name (print_IFL_requirement_E rqr);
      Printf.fprintf oc "LTLSPEC %s\n" (iflE_to_LTL rqr))
    iflreqs;
  Printf.fprintf oc "\n"

let print_NuSMV semantics oc =
  Printf.fprintf oc "%s\n" "MODULE main\n\n\n     DEFINE";
  print_attributes semantics.ta semantics.attributes oc;

  Printf.fprintf oc "\n";
  print_types semantics.types oc;

  Printf.fprintf oc "\n";
  print_operations semantics.bigo oc;

  Printf.fprintf oc "\n";
  print_allows semantics.allows oc semantics.types semantics.ta;

  Printf.fprintf oc "\n";
  print_requirements 
    (SM.bindings semantics.ifl)
    oc;

  close_out oc;
  print_endline "... NuSMV generated"

let temp = "NuSMVoutput"
let mc_in_file = "NuSMVinput"
let mc_out_file = "NuSMVoutput"

let _ =
  if Array.length Sys.argv != 2 then
    raise (UsageError "Argument <IFCIL-input-file> is needed");
  let in_file = open_in Sys.argv.(1) in
  let temp_out_file = open_out mc_in_file in
  let lexbuf = Lexing.from_channel in_file in
  let config' = 
    (SELinuxIFCIL.IFCILparser.main SELinuxIFCIL.IFCILlexer.token lexbuf) in
  let config =
    SELinuxIFCIL.IFCILsyntax.removeIN config' in
  close_in in_file;
  try
    (
    let semantics = get_semantics (config)
    in
    print_endline "... semantics computed";
    print_NuSMV semantics temp_out_file;
    let in_NuSMVfile = open_in mc_in_file in
    let requirements = parse_requirements_from_NuSMV in_NuSMVfile in
    close_in_noerr in_NuSMVfile;
    if
      print_endline "... verifying requirements";
      Sys.command ("./NuSMV  -v 0 " ^ mc_in_file ^ " > " ^ mc_out_file ^ " 2>/dev/null") <> 0 
    then
        failwith "problem running NuSMV"
    else
      let tmp_file = open_in mc_out_file in
      print_endline "\n+++ RESULTS +++\n";
      parse_response tmp_file requirements  
    )
  with 
    | Not_found -> (
        failwith "Not Found in computing semantics")
    | Failure s -> (
        failwith ("Failed computing semantics " ^ s))
      
  
open CILgrammar
open IFCILconfiguration
open IFL
open Sys
module SS = Set.Make (String)
module Dict = Map.Make (String)

let is_write c os =
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

let is_read c os =
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

let arrow_add os toadd ostof =
  try Dict.add os (toadd :: Dict.find os ostof) ostof
  with Not_found -> Dict.add os [ toadd ] ostof

let arrow_add_list (o, node) ostof =
  List.fold_left (fun ostf os -> arrow_add os node ostf) ostof o

let rec type_in_attr t expr typesreal attributesreal =
  let rec type_in_attr' t expr encounted =
    match expr with
    | None -> false
    | Some (A_NAME [ "all" ]) -> true
    | Some (A_NAME node) -> (
        if List.exists (fun n -> n = node) typesreal then node = t
        else
          (* Attribute definitions may be circular *)
          try
            if List.exists (fun at -> at = node) encounted then
              (* Using the function associated to the name encountered so far should be the expected result TODO *)
              type_in_attr' t None encounted
            else
              let _, exp' =
                List.find (fun (at, exp') -> at = node) attributesreal
              in
              type_in_attr' t exp' (node :: encounted)
          with Not_found ->
            if node <> [ "nonodeatall" ] then
              print_string ("not finding " ^ print_attrset (A_NAME node));
            (* you can have unused attributes, if no attributeset is given then no type is of such attribute *)
            false)
    | Some (A_NOT exp') -> not (type_in_attr' t (Some exp') encounted)
    | Some (A_AND (exp', exp'')) ->
        type_in_attr' t (Some exp') encounted
        && type_in_attr' t (Some exp'') encounted
    | Some (A_OR (exp', exp'')) ->
        type_in_attr' t (Some exp') encounted
        || type_in_attr' t (Some exp'') encounted
    | Some (A_XOR (exp', exp'')) ->
        (not (type_in_attr' t (Some exp') encounted))
        = type_in_attr' t (Some exp'') encounted
  in
  type_in_attr' t expr []

let rec types_of expr typesreal attributesreal =
  (* This function only uses tail resursive functions, but is not tail recoursive *)
  let rec f_types_of exp encounted =
    match exp with
    | None -> fun _ -> false
    | Some (A_NAME [ "all" ]) -> fun _ -> true
    | Some (A_NAME node) -> (
        if List.exists (fun n -> n = node) typesreal then fun x -> x = node
        else
          (* Attribute definitions may be circular *)
          try
            if List.exists (fun at -> at = node) encounted then
              (* Using the function associated to the name encountered so far should be the expected result TODO *)
              f_types_of None encounted
            else
              let _, exp' =
                List.find (fun (at, exp') -> at = node) attributesreal
              in
              f_types_of exp' (node :: encounted)
          with Not_found ->
            print_string ("not finding " ^ print_attrset (A_NAME node));
            raise Not_found)
    | Some (A_NOT exp') -> fun x -> not ((f_types_of (Some exp') encounted) x)
    | Some (A_AND (exp', exp'')) ->
        fun x ->
          f_types_of (Some exp') encounted x
          && f_types_of (Some exp'') encounted x
    | Some (A_OR (exp', exp'')) ->
        fun x ->
          f_types_of (Some exp') encounted x
          || f_types_of (Some exp'') encounted x
    | Some (A_XOR (exp', exp'')) ->
        fun x ->
          (not (f_types_of (Some exp') encounted x))
          = f_types_of (Some exp'') encounted x
  in
  List.filter (f_types_of expr []) typesreal

let name node = String.concat "#" (List.tl node)

let rec kind_to_LTL k type_nodes =
  let print_o o =
    let os = List.map (fun oi -> "operation = " ^ oi) o in
    if List.length os > 1 then "(" ^ String.concat " | " os ^ ")"
    else String.concat " | " os
  and print_type node =
    if node = [ "any-node" ] then "(! (state = pozzo))"
    else if SS.mem (name node) type_nodes then "state = " ^ name node
    else name node
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
      print_type n1 ^ " &  X (" ^ kind_to_LTL ks type_nodes ^ ")"
  | (n1, (o, SHORTARROW), n2) :: ks ->
      print_type n1 ^ " & " ^ print_o o ^ " &  X (" ^ kind_to_LTL ks type_nodes
      ^ ")"
  | (n1, ([ "any-mod" ], LONGARROW), n2) :: ks ->
      print_type n1 ^ " &  X ( F (" ^ kind_to_LTL ks type_nodes ^ "))"
  | (n1, (o, LONGARROW), n2) :: ks ->
      print_type n1 ^ " & " ^ print_o o ^ " &  X ( " ^ print_o o ^ " U ("
      ^ kind_to_LTL ks type_nodes ^ "))"

let ifl_to_LTL rqr type_nodes =
  match rqr with
  | MUST k | MUSTNOT k -> "!(" ^ kind_to_LTL k type_nodes ^ ")"
  | EVERYMUST (k1, k2) ->
      "(!(" ^ kind_to_LTL k1 type_nodes ^ ") | ("
      ^ kind_to_LTL (kind_meet k1 k2) type_nodes
      ^ "))"

let attr_to_smv ex type_nodes =
  let rec attr_to_smv' ex =
    match ex with
    | A_NAME [ "all" ] -> "TRUE"
    | A_NAME n ->
        if SS.exists (fun t -> t = name n) type_nodes then "state = " ^ name n
        else name n
    | A_NOT ex' -> "! ( " ^ attr_to_smv' ex' ^ ")"
    | A_AND (ex', ex'') ->
        "( " ^ attr_to_smv' ex' ^ " & " ^ attr_to_smv' ex'' ^ ")"
    | A_OR (ex', ex'') ->
        "( " ^ attr_to_smv' ex' ^ " | " ^ attr_to_smv' ex'' ^ ")"
    | A_XOR (ex', ex'') ->
        "( " ^ attr_to_smv' ex' ^ " xor " ^ attr_to_smv' ex'' ^ ")"
  in
  "(" ^ attr_to_smv' ex ^ ") & ! (state = pozzo)"

let print_NuSMV fstmntls oc =
  (* List of full names of types *)
  let fstmntls =
    remove_recursion (([ "#" ], FLATATTRIBUTE "nonodeatall") :: fstmntls)
  in
  let typesreal =
    List.fold_left
      (fun nds fstmnt ->
        match fstmnt with
        | pos, FLATTYPE a ->
            if pos = [ "#" ] || is_block pos fstmntls then (pos @ [ a ]) :: nds
            else nds
        | _ -> nds)
      [] fstmntls
  in
  print_string "typesreal computed\n";
  (* Set of node names of types *)
  let type_nodes =
    List.fold_left
      (fun nds fstmnt ->
        match fstmnt with
        | pos, FLATTYPE a ->
            if pos = [ "#" ] || is_block pos fstmntls then
              SS.add (name (pos @ [ a ])) nds
            else nds
        | _ -> nds)
      SS.empty fstmntls
  in
  print_string "typenodes computed\n";
  (* List of pairs (full name of attribute, expression defining the attribute) *)
  let attributesreal =
    List.fold_left
      (fun nds fstmnt ->
        match fstmnt with
        | pos, FLATATTRIBUTE a ->
            if pos = [ "#" ] || is_block pos fstmntls then
              match
                List.fold_left
                  (fun ls (pos', flstm) ->
                    match flstm with
                    | FLATATTRIBUTESET (n, ex) ->
                        if n = pos @ [ a ] then ex :: ls else ls
                    | _ -> ls)
                  [] fstmntls
              with
              | [] -> (pos @ [ a ], None) :: nds
              | [ ex ] -> (pos @ [ a ], Some ex) :: nds
              | ex :: ls ->
                  let exx =
                    List.fold_left (fun ex' ex'' -> A_OR (ex', ex'')) ex ls
                  in
                  (pos @ [ a ], Some exx) :: nds
            else nds
        | _ -> nds)
      [] fstmntls
  in
  let attributes =
    print_string "attributesreal computed\n";
    (* List of pairs (node name of attribute, string NuSMV expression defining the attribute) *)
    List.fold_left
      (fun nds fstmnt ->
        match fstmnt with
        | pos, FLATATTRIBUTE a ->
            if pos = [ "#" ] || is_block pos fstmntls then
              match
                List.fold_left
                  (fun ls (pos', flstm) ->
                    match flstm with
                    | FLATATTRIBUTESET (n, ex) ->
                        if n = pos @ [ a ] then ex :: ls else ls
                    | _ -> ls)
                  [] fstmntls
              with
              | [] -> (name (pos @ [ a ]), "FALSE") :: nds
              | [ ex ] -> (name (pos @ [ a ]), attr_to_smv ex type_nodes) :: nds
              | ex :: ls ->
                  let exx =
                    List.fold_left (fun ex' ex'' -> A_OR (ex', ex'')) ex ls
                  in
                  (name (pos @ [ a ]), attr_to_smv exx type_nodes) :: nds
            else nds
        | _ -> nds)
      [] fstmntls
  in
  let labeledrequirements =
    print_string "attributes computed\n";
    (* List of all the requirements (only the ones to consider, in blocks or in global environment) *)
    List.fold_left
      (fun rqs fstmnt ->
        match fstmnt with
        | pos, FLATIFL (lbl, req) ->
            if pos = [ "#" ] || is_block pos fstmntls then (lbl, req) :: rqs
            else rqs
        | _ -> rqs)
      [] fstmntls
  in
  let requirements = List.map snd labeledrequirements in
  print_string "requirements computed\n";
  let add_dst src dst ops pos dic =
    let srcn = name src and dstn = name dst in
    let pairs = List.map (fun op -> (dstn, op)) ops in
    if is_type dst fstmntls then
      try
        let oldv = Dict.find srcn dic in
        Dict.add srcn (List.rev_append pairs (fst oldv), snd oldv) dic
      with Not_found -> Dict.add srcn (pairs, []) dic
    else if is_attribute dst fstmntls then
      try
        let oldv = Dict.find srcn dic in
        Dict.add srcn (fst oldv, List.rev_append pairs (snd oldv)) dic
      with Not_found -> Dict.add srcn ([], pairs) dic
    else
      raise
        (UndefinedReference
           (String.concat "." dst ^ "is not a type nor an attribute"))
  in
  let att_arcs, type_arcs, operationset =
    List.fold_left
      (fun (da, dt, oset) fstmt ->
        match fstmt with
        | pos, FLATALLOW (src, dst, clsper) ->
            if pos = [ "#" ] || is_block pos fstmntls then
              let ops = operations clsper fstmntls in
              let oset' =
                List.fold_left
                  (fun oss o -> SS.add o oss)
                  oset (List.map snd ops)
              in
              let write_os =
                (* list of write-like operations defined in clsper *)
                List.rev_map
                  (fun (a, b) -> b)
                  (List.filter (fun (cs, os) -> is_write cs os) ops)
              and read_os =
                List.rev_map
                  (fun (a, b) -> b)
                  (List.filter (fun (cs, os) -> is_read cs os) ops)
              in
              let da', dt' =
                if is_type src fstmntls then
                  (da, add_dst src dst write_os pos dt)
                else (add_dst src dst write_os pos da, dt)
              in
              if is_type dst fstmntls then
                (da', add_dst dst src read_os pos dt', oset')
              else (add_dst dst src read_os pos da', dt', oset')
            else (da, dt, oset)
        | _, _ -> (da, dt, oset))
      (Dict.empty, Dict.empty, SS.empty)
      fstmntls
  in
  let operationset = all_defined_operations fstmntls in
  print_string "generating NuSMV configuration\n";
  Printf.fprintf oc "%s\n" "MODULE main\n\n\n     DEFINE";
  List.iter (fun (att, ex) -> Printf.fprintf oc "%s := %s;\n" att ex) attributes;
  Printf.fprintf oc "\n";
  Printf.fprintf oc "\n\n     VAR\n\n         state : { pozzo, %s  };\n\n"
    (String.concat ",\n              "
       (List.sort_uniq String.compare (SS.elements type_nodes)));

  Printf.fprintf oc "\n";
  Printf.fprintf oc "\n\n     IVAR\n\n         operation : { %s  };\n"
    (String.concat ",\n              "
       (List.sort_uniq String.compare (SS.elements operationset)));

  Printf.fprintf oc "\n";
  Printf.fprintf oc "\n\n    TRANS\n";

  List.iter
    (fun t ->
      (Printf.fprintf oc "(state = %s -> ( " t;
       try
         let type_ars_t = Dict.find t type_arcs in
         List.iter
           (fun (d, o) ->
             Printf.fprintf oc "(operation = %s & next(state = %s)) | " o d)
           (fst type_ars_t);

         List.iter
           (fun (d, o) ->
             Printf.fprintf oc "(operation = %s & next(%s)) | " o d)
           (snd type_ars_t)
       with Not_found -> Printf.fprintf oc "");
      Printf.fprintf oc " next(state = pozzo) )) &\n")
    (SS.elements type_nodes);

  Printf.fprintf oc "(state = pozzo -> next(state = pozzo))\n\n\n";
  List.iter
    (fun (name, rqr) ->
      Printf.fprintf oc "--  %s\n" name;
      Printf.fprintf oc "LTLSPEC %s\n" (ifl_to_LTL rqr type_nodes))
    labeledrequirements;
  Printf.fprintf oc "\n";
  close_out oc;
  print_string "NuSMV generated\n";
  (labeledrequirements, type_nodes)

let parse_response_line line mustfalse =
  try
    if String.sub line 0 16 = "-- specification" then
      try
        if
          (String.sub line (String.length line - 8) 8 = "is false" && mustfalse)
          || String.sub line (String.length line - 7) 7 = "is true"
             && not mustfalse
        then "-------- is verified!\n\n"
        else "-------- is NOT verified!\n\n"
      with Invalid_argument s ->
        raise (Invalid_argument (s ^ " when parsing responses"))
    else ""
  with Invalid_argument s -> ""

let rec parse_response oc' requirements =
  if requirements <> [] then (
    try
      let mustfalse =
        match snd (List.hd requirements) with
        | MUST _ -> true
        | MUSTNOT _ | EVERYMUST _ -> false
      in
      let line = input_line oc' in
      let result = parse_response_line line mustfalse in
      (* print_string (line ^ "\n"); *)
      if result <> "" then (
        print_string
          ("++++ ("
          ^ fst (List.hd requirements)
          ^ ") "
          ^ print_IFL_requirement (snd (List.hd requirements))
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

let verify config infMC outfMC =
  let start = Sys.time () in
  let oc = open_out infMC and oc' = open_in outfMC in
  let requirements, type_nodes = print_NuSMV config oc in
  print_newline ();
  flush stdout;
  print_string "\n\n";
  print_newline ();
  flush stdout;
  Sys.command ("./NuSMV " ^ infMC ^ " > " ^ outfMC);
  parse_response oc' requirements;
  let stop = Sys.time () in
  Printf.printf "Execution time: %fs\n%!" (stop -. start);
  close_in_noerr oc'

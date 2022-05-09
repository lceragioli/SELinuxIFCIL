open CILsyntax
open CILgrammar
open Utils

module SS = Set.Make (String)

module StringList = OrderList (String)
module SLS = Set.Make (StringList)

module StSL = OrderPair (String) (StringList)
module StSLS = Set.Make (StSL)

module SM = Map.Make (String)
module SLM = Map.Make (StringList)

exception OurError of string

exception UnsupportedConstruct of string

exception UndefinedReference of string

type flat_statement =
  | FLATTYPE of string
  | FLATTYPEALIAS of string
  | FLATTYPEALIASACTUAL of string * string list
  | FLATATTRIBUTE of string
  | FLATATTRIBUTESET of path * attributeexp
  | FLATBLOCKABSTRACT
  | FLATBLOCK of string
  | FLATBLOCKINHERIT of path * refinements
  | FLATCALL of path * path list * refinements
  | FLATMACRO of string * (parametertype * string) list
  | FLATALLOW of path * path * classpermission
  | FLATIFL of string * iflreq
  | FLATCOMMON of string * string list
  | FLATCLASSCOMMON of string list * string list
  | FLATCLASS of string * string list
  | FLATCLASSPERMISSION of string
  | FLATCLASSPERMISSIONSET of string list * string list * classpermissionsetcon
  | FLATCLASSMAP of string * string list
  | FLATCLASSMAPPING of string list * string list * classpermission

(* ------------------- name resolution ------------------- *)

let rec eval_macr pos construct qname fstmntls =
  (* eval_macr pos construct qname fstmntls
     parameters:
       pos - the path in which the name to be resolved occurs
       construct - the kind of construct we are resolving, may be a type, a typeattribute etc
       qname - the name to be resolved, it is a full name, may be A.c.d, #.B.d, d, etc
       fstmntls - the consfiguration we are considering

     return the real name of the entity of kind construct named with qname appearing in pos WHICH IS A MACRO, i.e., a name starting with #
  *)
  let resolve qual condmaps =
    match
      list_find_map
        (fun ps ->
          list_find_map
            (fun (cond, map) ->
              if
                List.exists
                  (fun (ps', i) -> ps' = ps @ qual && cond i qual)
                  fstmntls
              then Some (map (ps @ qname))
              else None)
            condmaps)
        (if pos = [ "#" ] then [ pos ] else [ pos ])
    with
    | Some fqname -> fqname
    | None ->
        raise
          (UndefinedReference
             (String.concat "." qname ^ " in " ^ String.concat "." pos))
  in
  match qname with
  | "#" :: bs -> qname
  | _ -> (
      let name, qual = last_and_list qname in
      match construct with
      | BLOCK -> (
          try resolve qual [ ((fun i _ -> i = FLATBLOCK name), id) ]
          with UndefinedReference s ->
            raise (UndefinedReference ("block: " ^ s)))
      | TYPE -> (
          try
            resolve qual
              [
                ((fun i _ -> i = FLATTYPE name), id);
                ( (fun i qual -> i = FLATTYPEALIAS name && qual = []),
                  fun qname ->
                    let name, qual = last_and_list qname in
                    let actual =
                      List.find_opt
                        (fun (ps, i) ->
                          ps = qual
                          &&
                          match i with
                          | FLATTYPEALIASACTUAL (n, t) -> n = name
                          | _ -> false)
                        fstmntls
                    in
                    match actual with
                    | Some (ps, FLATTYPEALIASACTUAL (n, t)) ->
                        eval_macr ps TYPE t fstmntls
                    | _ ->
                        raise
                          (UndefinedReference
                             (String.concat "type " (pos @ qname))) );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("type: " ^ s))
            (* if
                 List.exists
                   (fun (ps, i) -> ps = pos @ qual && i = FLATTYPE name)
                   fstmntls
               then pos @ qname
               else if
                 qual = []
                 && List.exists
                      (fun (ps, i) -> ps = pos @ qual && i = FLATTYPEALIAS name)
                      fstmntls
               then
                 let actual =
                   List.find_opt
                     (fun (ps, i) ->
                       ps = pos @ qual
                       &&
                       match i with
                       | FLATTYPEALIASACTUAL (n, t) -> n = name
                       | _ -> false)
                     fstmntls
                 in
                 match actual with
                 | Some (ps, FLATTYPEALIASACTUAL (n, t)) ->
                     eval_bl ps TYPE t fstmntls
                 | _ ->
                 raise (UndefinedReference (String.concat "type " (pos @ qname)))

               else
                 raise (UndefinedReference (String.concat "type " (pos @ qname))) *)
          )
      | ATTRIBUTE -> (
          try resolve qual [ ((fun i _ -> i = FLATATTRIBUTE name), id) ]
          with UndefinedReference s ->
            raise (UndefinedReference ("attribute: " ^ s)))
      | MACRO -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with FLATMACRO (n, _) -> n = name | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("macro: " ^ s)))
      | COMMON -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with FLATCOMMON (n, _) -> n = name | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("common: " ^ s)))
      | CLASS -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with FLATCLASS (n, _) -> n = name | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("class: " ^ s)))
      | CLASSMAP -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with FLATCLASSMAP (n, _) -> n = name | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("classmap: " ^ s)))
      | CLASSPERMISSION -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with
                    | FLATCLASSPERMISSION n -> n = name
                    | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("classmap: " ^ s)))
      (* Not needed: classcommon, classmapping, classpermissionset *)
      | CLASSCOMMON | CLASSPERMISSIONSET | CLASSMAPPING | _ -> qname)

let rec eval_bl pos construct qname fstmntls =
  (* let rec eval_bl pos construct qname fstmntls
     parameters:
       pos - the path in which the name to be resolved occurs
       construct - the kind of construct we are resolving, may be a type, a typeattribute etc
       qname - the name to be resolved, it is a full name, may be A.c.d, #.B.d, d, etc
       fstmntls - the consfiguration we are considering

     return the real name of the entity of kind construct named with qname appearing in pos WHICH IS A BLOCK, i.e., a name starting with #
  *)
  let resolve qual condmaps =
    match
      list_find_map
        (fun ps ->
          list_find_map
            (fun (cond, map) ->
              if
                List.exists
                  (fun (ps', i) -> ps' = ps @ qual && cond i qual)
                  fstmntls
              then Some (map (ps @ qname))
              else None)
            condmaps)
        (if pos = [ "#" ] then [ pos ]
        else List.map (fun prf -> "#" :: prf) (prefix_list (List.tl pos)))
    with
    | Some fqname -> fqname
    | None ->
        raise
          (UndefinedReference
             (String.concat "." qname ^ " in " ^ String.concat "." pos))
  in
  match qname with
  | "#" :: bs -> qname
  | _ -> (
      let name, qual = last_and_list qname in
      match construct with
      | BLOCK -> (
          try resolve qual [ ((fun i _ -> i = FLATBLOCK name), id) ]
          with UndefinedReference s ->
            raise (UndefinedReference ("block: " ^ s)))
      | TYPE -> (
          try
            resolve qual
              [
                ((fun i _ -> i = FLATTYPE name), id);
                ( (fun i qual -> i = FLATTYPEALIAS name && qual = []),
                  fun qname ->
                    let name, qual = last_and_list qname in
                    let actual =
                      List.find_opt
                        (fun (ps, i) ->
                          ps = qual
                          &&
                          match i with
                          | FLATTYPEALIASACTUAL (n, t) -> n = name
                          | _ -> false)
                        fstmntls
                    in
                    match actual with
                    | Some (ps, FLATTYPEALIASACTUAL (n, t)) ->
                        eval_bl ps TYPE t fstmntls
                    | _ ->
                        raise
                          (UndefinedReference
                             (String.concat "type " (pos @ qname))) );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("type: " ^ s))
            (* if
                 List.exists
                   (fun (ps, i) -> ps = pos @ qual && i = FLATTYPE name)
                   fstmntls
               then pos @ qname
               else if
                 qual = []
                 && List.exists
                      (fun (ps, i) -> ps = pos @ qual && i = FLATTYPEALIAS name)
                      fstmntls
               then
                 let actual =
                   List.find_opt
                     (fun (ps, i) ->
                       ps = pos @ qual
                       &&
                       match i with
                       | FLATTYPEALIASACTUAL (n, t) -> n = name
                       | _ -> false)
                     fstmntls
                 in
                 match actual with
                 | Some (ps, FLATTYPEALIASACTUAL (n, t)) ->
                     eval_bl ps TYPE t fstmntls
                 | _ ->
                 raise (UndefinedReference (String.concat "type " (pos @ qname)))

               else
                 raise (UndefinedReference (String.concat "type " (pos @ qname))) *)
          )
      | ATTRIBUTE -> (
          try resolve qual [ ((fun i _ -> i = FLATATTRIBUTE name), id) ]
          with UndefinedReference s ->
            raise (UndefinedReference ("attribute: " ^ s)))
      | MACRO -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with FLATMACRO (n, _) -> n = name | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("macro: " ^ s)))
      | COMMON -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with FLATCOMMON (n, _) -> n = name | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("common: " ^ s)))
      | CLASS -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with FLATCLASS (n, _) -> n = name | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("class: " ^ s)))
      | CLASSMAP -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with FLATCLASSMAP (n, _) -> n = name | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("classmap: " ^ s)))
      | CLASSPERMISSION -> (
          try
            resolve qual
              [
                ( (fun i _ ->
                    match i with
                    | FLATCLASSPERMISSION n -> n = name
                    | _ -> false),
                  id );
              ]
          with UndefinedReference s ->
            raise (UndefinedReference ("classpermission: " ^ s)))
      (* Not needed: classcommon, classmapping, classpermissionset *)
      | CLASSCOMMON | CLASSPERMISSIONSET | CLASSMAPPING | _ -> qname)

let eval_bl_type_attr pos construct name fstmntls =
  (* let eval_bl_type_attr pos construct name fstmntls
     parameters:
       pos - the path in which the name to be resolved occurs
       construct - in lots of contexts, types and typeattributes can appear one in place oif another;
         if construct is TYPE than we are looking for a type and if not defined for an attribute, if construct is TYPEATTRIBUTE than we are looking for an attribute
       name - the name to be resolved, it is a (relative or absolute) full name, may be A.c.d, #.B.d, d, etc
       fstmntls - the consfiguration we are considering

     return the real name of the entity of kind construct named with qname appearing in pos WHICH IS A BLOCK, i.e., a name starting with #
  *)
  if construct = TYPE then
    (* TODO: check, this is done globally and not layer by layer in the nesting of pos, is it right? *)
    try eval_bl pos TYPE name fstmntls
    with UndefinedReference _ -> (
      try eval_bl pos ATTRIBUTE name fstmntls
      with UndefinedReference _ ->
        raise
          (UndefinedReference
             ("eval_bl_type_attr - " ^ String.concat "." name ^ " in "
            ^ String.concat "." pos)))
  else eval_bl pos ATTRIBUTE name fstmntls

(* ------------------- definition checking ------------------- *)

let is_block qname fstmntls =
  (* is_block qname fstmntls
     parameters:
       qname - absolutely qualified name of the BLOCK, i.e., #.A.B.C
       fstmntls - consfiguration in hand

     returns true if such a block is defined in the configuration
  *)
  let name, path = last_and_list qname in
  List.exists
    (fun p ->
      match p with qname, FLATBLOCK n -> qname = path && n = name | _ -> false)
    fstmntls

let is_type qname fstmntls =
  (* is_type t pos fstmntls
     parameters:
       qname - absolutely qualified name of the TYPE, i.e., #.A.B.a
       fstmntls - consfiguration in hand

     returns true if such a type is defined in the configuration
  *)
  let n, pos = last_and_list qname in
  (* TODO: does not consider typealias, is it right? *)
  List.exists
    (fun (pos', i) ->
      pos' = pos && match i with FLATTYPE n' -> n' = n | _ -> false)
    fstmntls

let is_attribute t fstmntls =
  (* is_type t pos fstmntls
     parameters:
       qname - absolutely qualified name of the TYPATTRIBUTE, i.e., #.A.B.a
       fstmntls - consfiguration in hand

     returns true if such a typeattribute is defined in the configuration
  *)
  let n, pos = last_and_list t in
  List.exists
    (fun (pos', i) ->
      pos' = pos && match i with FLATATTRIBUTE n' -> n' = n | _ -> false)
    fstmntls

(* ------------------- normalization ------------------- *)

let rec remove_recursion fstmntls =
  (* remove_recursion fstmntls
      returns an equivalent versione of the configuration in input where undefined recursion in the definitions
      of attributes is removed
  *)
  (* TODO: solve non trivial cases *)
  let rec reachable at' ex fstmntls' =
    match ex with
    | A_NAME n ->
        if n = at' then true
        else false
          (* List.exists
                 (fun stm ->
                   match stm with
                   | (_, FLATATTRIBUTESET (attr, expr)) when attr = n ->
                       reachable at' expr fstmntls'
                   | _ -> false
                 )
             fstmntls' *)
    | A_NOT ex' -> reachable at' ex' fstmntls'
    | A_OR (ex1, ex2) ->
        reachable at' ex1 fstmntls' || reachable at' ex2 fstmntls'
    | A_AND (ex1, ex2) ->
        reachable at' ex1 fstmntls' || reachable at' ex2 fstmntls'
    | A_XOR (ex1, ex2) ->
        reachable at' ex1 fstmntls' || reachable at' ex2 fstmntls'
  in
  let rec remove_recursion_expr at ex =
    match ex with
    | A_NAME n ->
        if at = n then (
          print_string "removing recursion\n";
          A_NAME [ "#"; "nonodeatall" ])
        else ex
    | A_NOT ex' -> A_NOT (remove_recursion_expr at ex')
    | A_OR (ex1, ex2) ->
        A_OR (remove_recursion_expr at ex1, remove_recursion_expr at ex2)
    | A_AND (ex1, ex2) ->
        A_AND (remove_recursion_expr at ex1, remove_recursion_expr at ex2)
    | A_XOR (ex1, ex2) ->
        A_XOR (remove_recursion_expr at ex1, remove_recursion_expr at ex2)
  in
  List.fold_left
    (fun fstmntls' fstmnt ->
      match fstmnt with
      | pos, FLATATTRIBUTESET (at, ex) ->
          List.rev_map
            (fun fstmnt ->
              match fstmnt with
              | pos', FLATATTRIBUTESET (at', ex')
                when at = at' || reachable at' ex fstmntls' ->
                  (pos', FLATATTRIBUTESET (at', remove_recursion_expr at ex'))
              | _ -> fstmnt)
            fstmntls'
      | _ -> fstmntls')
    fstmntls fstmntls

(* ------------------- metadata access ------------------- *)

let all_defined_operations fstmntls =
  (* returns the set of the names of all the defined operations in the input configuration *)
  List.fold_left
    (fun ss fstmt ->
      match fstmt with
      | _, FLATCLASS (cls, perms) ->
          List.fold_left (fun ss' p -> SS.add p ss') ss perms
      | _, FLATCOMMON (com, perms) ->
          List.fold_left (fun ss' p -> SS.add p ss') ss perms
      | _ -> ss)
    SS.empty fstmntls

let rec filter_operations alloperations exp =
  (* given a set of operations and an expression
     returns the subset of operations defined bu the expression
  *)
  match exp with
  | A_NAME n when n = [ "all" ] -> alloperations
  | A_NAME n ->
      if List.length n > 1 then
        raise (UnsupportedConstruct "path of permissions")
      else if SS.exists (fun e -> e = List.hd n) alloperations then
        SS.add (List.hd n) SS.empty
      else SS.empty
  | A_AND (ex1, ex2) ->
      let op1 = filter_operations alloperations ex1
      and op2 = filter_operations alloperations ex2 in
      SS.inter op1 op2
  | A_OR (ex1, ex2) ->
      let op1 = filter_operations alloperations ex1
      and op2 = filter_operations alloperations ex2 in
      SS.union op1 op2
  | A_XOR (ex1, ex2) ->
      let op1 = filter_operations alloperations ex1
      and op2 = filter_operations alloperations ex2 in
      SS.diff (SS.union op1 op2) (SS.inter op1 op2)
  | A_NOT ex' -> SS.diff alloperations (filter_operations alloperations ex')

let operations_of_clspermsetcon clssetcon cls fstmntls =
  (* take the operation for cls and filter them using clssetcon *)
  let alloperations =
    slist_to_ss
      (List.flatten
         (List.filter_map
            (fun fstmnt ->
              match fstmnt with
              | pos, FLATCLASS (c, perms) ->
                  if pos @ [ c ] = cls then Some perms else None
              | _, FLATCLASSCOMMON (c, com) ->
                  if c = cls then
                    Some
                      (List.flatten
                         (List.filter_map
                            (fun fstmnt' ->
                              match fstmnt' with
                              | pos', FLATCOMMON (name, ops) ->
                                  if com = pos' @ [ name ] then Some ops
                                  else None
                              | _ -> None)
                            fstmntls))
                  else None
              | _ -> None)
            fstmntls))
  in
  match clssetcon with
  | Permissions pls -> pls
  | Expression exp -> SS.elements (filter_operations alloperations exp)
(* ["read"; "write"] *)

let ops_in_class qname fstmntls =
  (* 
  ops_in_class qname fstmntls
  parameters:
    qname - a absolutly qualified name of a CLASS
    fstmntls - a configuration

  returns the list of operations defined on the class
  *)
  let n, pos' = last_and_list qname in
  let ops =
    List.flatten
      (List.filter_map
         (fun fstmnt ->
           match fstmnt with
           | pos, FLATCLASS (c, perms) ->
               if pos = pos' && n = c then Some perms else None
           | _, FLATCLASSCOMMON (c, com) ->
               if c = qname then
                 Some
                   (List.flatten
                      (List.filter_map
                         (fun fstmnt' ->
                           match fstmnt' with
                           | pos', FLATCOMMON (name, ops) ->
                               if com = pos' @ [ name ] then Some ops else None
                           | _ -> None)
                         fstmntls))
               else None
           | _ -> None)
         fstmntls)
  in
  if ops = [] then None else Some ops

let rec operations clsper fstmntls =
  (* 
  Given a classpermission and a configuration, 
  returns the list of pairs (cls, operation) of classes and operations defined on them 
  *)
  let ops_in_classmapping clsmap clsmapping fstmntls =
    match
      list_find_map
        (fun fstmnt ->
          match fstmnt with
          | _, FLATCLASSMAPPING (clsmap', clsmapping', clsperms)
            when clsmap = clsmap' && clsmapping = clsmapping' ->
              Some (operations clsperms fstmntls)
          | _ -> None)
        fstmntls
    with
    | Some ops -> ops
    | None -> raise (UndefinedReference "mapping undefined")
  in

  match clsper with
  | Anonym (cls, perm) -> (
      match ops_in_class cls fstmntls with
      | Some ops' -> (
          match perm with
          | Permissions ops ->
              print_string ("permissions!!\n\n" ^ String.concat ", " ops);
              List.map (fun o -> (cls, o)) ops
          | Expression ops ->
              List.map
                (fun o -> (cls, o))
                (SS.elements (filter_operations (slist_to_ss ops') ops)))
      | None -> (
          (* otherwise it must be a classmap *)
          match perm with
          | Permissions clsmapping when List.length clsmapping = 1 ->
              ops_in_classmapping cls clsmapping fstmntls
          | Expression ops -> (
              match ops with
              | A_NAME mapping -> ops_in_classmapping cls mapping fstmntls
              | _ ->
                  raise
                    (UnsupportedConstruct
                       "mapping with permission expression in allow"))
          | _ ->
              raise
                (UnsupportedConstruct
                   "mapping with permission expression in allow")))
  | Name clspermset -> (
      (* it is a classpermission - just resolve and take the permissions *)
      let set =
        List.find
          (fun fstm ->
            match fstm with
            | _, FLATCLASSPERMISSIONSET (clspermset', cls, clssetcon) ->
                clspermset' = clspermset
            | _ -> false)
          fstmntls
      in
      match set with
      | _, FLATCLASSPERMISSIONSET (clspermset', cls, clssetcon) ->
          List.map
            (fun o -> (cls, o))
            (operations_of_clspermsetcon clssetcon cls fstmntls)
      | _ -> raise Not_found)

(* -------------- Printing functions -------------- *)

let print_IFL (node, marrow, node') =
  let marrowstring =
    match marrow with
    | m, CILsyntax.LONGARROW -> " -[" ^ String.concat "," m ^ "]> "
    | m, CILsyntax.SHORTARROW -> " [" ^ String.concat "," m ^ "]> "
  in
  " ( " ^ String.concat "." node ^ ", " ^ marrowstring ^ ", "
  ^ String.concat "." node' ^ " ) "

let print_IFL_requirement r =
  match r with
  | CILsyntax.MUST iflpath ->
      ".IFL-must "
      ^ List.fold_right (fun p s -> print_IFL p ^ s) iflpath ""
      ^ "\n"
  | CILsyntax.MUSTNOT iflpath ->
      ".IFL-mustnot "
      ^ List.fold_right (fun p s -> print_IFL p ^ s) iflpath ""
      ^ "\n"
  | CILsyntax.EVERYMUST (iflpath, iflpath') ->
      ".IFL-every "
      ^ List.fold_right (fun p s -> print_IFL p ^ s) iflpath ""
      ^ " must be "
      ^ List.fold_right (fun p s -> print_IFL p ^ s) iflpath' ""
      ^ "\n"

let print_IFL_refinements req =
  String.concat "\n"
    (List.map
       (fun (lbl, lbl', ifl) ->
         "(IFL (" ^ lbl ^ " - " ^ String.concat "." lbl' ^ ") "
         ^ print_IFL_requirement ifl ^ ")")
       req)

let rec print_attrset expr =
  match expr with
  | A_NAME n -> String.concat "." n
  | A_AND (e, e') -> "(" ^ print_attrset e ^ ") and (" ^ print_attrset e' ^ ")"
  | A_OR (e, e') -> "(" ^ print_attrset e ^ ") or (" ^ print_attrset e' ^ ")"
  | A_XOR (e, e') -> "(" ^ print_attrset e ^ ") xor (" ^ print_attrset e' ^ ")"
  | A_NOT e -> "not (" ^ print_attrset e ^ ")"

let print_path path = String.concat "." path

let print_classpermissionsetcon clspermcon =
  match clspermcon with
  | Permissions perms -> " permissions: " ^ String.concat " " perms
  | Expression expr -> " expression: " ^ print_attrset expr

let print_classpermission clsperm =
  match clsperm with
  | Name n -> " named: " ^ print_path n
  | Anonym (c, perms) ->
      " anonym: " ^ print_path c ^ " " ^ print_classpermissionsetcon perms

let print_fparams fparams =
  String.concat " "
    (List.map
       (fun (ptype, name) ->
         (match ptype with
         | PARTYPE -> "type"
         | PARTYPEALIAS -> "typealias"
         | PARCLASS -> "class"
         | PARCLASSMAP -> "classmap"
         | PARCLASSPERMISSION -> "classpermission"
         | PARIGNORE -> "ignore")
         ^ " " ^ name)
       fparams)

let rec print_flat_CIL (pos, stmnt) =
  print_path pos
  ^
  match stmnt with
  | FLATTYPE t -> ".type(" ^ t ^ ")"
  | FLATTYPEALIAS t -> ".typealias(" ^ t ^ ")"
  | FLATTYPEALIASACTUAL (t, t') -> ".typealias(" ^ t ^ print_path t' ^ ")"
  | FLATATTRIBUTE t -> ".typeattribute(" ^ t ^ ")"
  | FLATATTRIBUTESET (n, expr) ->
      ".typeattributeset(" ^ print_path n ^ ", " ^ print_attrset expr ^ ")"
  | FLATBLOCK b -> ".block(" ^ b ^ ")"
  | FLATBLOCKABSTRACT -> ".blockabstract"
  | FLATBLOCKINHERIT (b, i) ->
      ".inherit(" ^ print_path b ^ "\n" ^ print_IFL_refinements i ^ ")"
  | FLATCALL (n, p, i) ->
      ".call(" ^ print_path n ^ " ("
      ^ String.concat " " (List.map (fun param -> print_path param) p)
      ^ ")" ^ "\n" ^ print_IFL_refinements i ^ ")"
  | FLATMACRO (n, p) -> ".macro " ^ n ^ "(" ^ print_fparams p ^ ")"
  | FLATALLOW (src, trg, clsperm) ->
      ".allow(" ^ print_path src ^ " " ^ print_path trg ^ " ("
      ^ print_classpermission clsperm
      ^ "))"
  | FLATIFL (lbl, r) -> print_IFL_requirement r
  | FLATCOMMON (name, perms) ->
      ".common " ^ name ^ " ( " ^ String.concat " " perms ^ " )"
  | FLATCLASSCOMMON (cls, com) ->
      ".classcommon " ^ print_path cls ^ print_path com
  | FLATCLASS (cls, perms) ->
      ".class " ^ cls ^ "(" ^ String.concat " " perms ^ ")"
  | FLATCLASSPERMISSION n -> ".classpermission " ^ n
  | FLATCLASSPERMISSIONSET (clsperm, cls, perms) ->
      ".classpermissionset " ^ print_path clsperm ^ " " ^ print_path cls ^ " "
      ^ print_classpermissionsetcon perms
  | FLATCLASSMAP (clsmap, clsmappings) ->
      ".classmap " ^ clsmap ^ " ( " ^ String.concat " " clsmappings ^ ")"
  | FLATCLASSMAPPING (clsmap, clsmapping, clsperm) ->
      ".classmapping " ^ print_path clsmap ^ " " ^ print_path clsmapping
      ^ print_classpermission clsperm

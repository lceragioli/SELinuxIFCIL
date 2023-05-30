open CILsyntax
open IFCILconfiguration
open Utils
open IFL
module SS = Set.Make (String)

exception UndefinedMeet of string

exception NotUniqueMeet of string

exception UncorrectRefinement of string

(* let ifl_refine ns (FLATIFL(lbl, req)) i =
   try
     match List.hd
             (List.filter
               (fun (lbl'', lbl', req') -> lbl' = ns @ [lbl]) i)
     with
     | (lbl'', _, req') -> (FLATIFL(lbl'', meet req req'))
   with
     Failure s -> if s = "hd" then (FLATIFL(lbl, req)) else raise (Failure s) *)

let ifl_refine ns (FLATIFL (lbl, req)) i =
  try
    match
      List.hd (List.filter (fun (lbl'', lbl', req') -> lbl' = ns @ [ lbl ]) i)
    with
    | lbl'', _, req' ->
        if minor req' req then FLATIFL (lbl'', req')
        else raise (UncorrectRefinement "")
  with Failure s -> if s = "hd" then FLATIFL (lbl, req) else raise (Failure s)

let phase1 fstmntls =
  (* returns a new IFCIL configuration where the names of blocks in blockinherit statements are evaluated *)
  print_string "phase 1\n";
  flush stdout;
  let result =
    List.rev_map
      (fun fstmnt ->
        match fstmnt with
        | pos, FLATBLOCKINHERIT (b, i) ->
            ( pos,
              FLATBLOCKINHERIT
                ( (try eval_bl pos BLOCK b fstmntls
                   with UndefinedReference _ ->
                     eval_bl [ "#" ] BLOCK b fstmntls),
                  i ) )
        | fstmt -> fstmt)
      fstmntls
  in
  (* List.iter (fun i -> print_string (print_flat_CIL i ^ "\n")) result; *)
  List.sort_uniq compare result

let rec sort_inherits lsorted lunsorted =
  (* an inherit is a pair of paths (p1, p2, ref) such that the block at p1 inherits from 
     the block at p2 with refinements ref
     lsorted - inherits sorted according to the partial order of inherits
     lunsorted - non sorted inherits
     returns a sorted list of the inherits in the two lists *)
  let first, rest =
    List.partition
      (fun (pos, block, ref) ->
        List.for_all
          (fun (pos2, block2, ref2) ->
            listminus pos2 block = None)
          lunsorted)
      lunsorted
    in
  if lunsorted = [] then lsorted else
    sort_inherits (List.rev_append (List.rev lsorted) first) rest
  
let phase2 fstmntls =
  (* returns a new IFCIL configuration where the content of inherited blocks
     is copied on the inheriting blocks.
     Inheriting from a block that inherits something is resolved by sorting them according
     to the partial order of inheritance *)
  print_string "phase 2\n";
  flush stdout;
  let inherits =
    (* an inherit is a pair of paths (p1, p2) such that the block at p1 inherits from the block at p2 *)
    sort_inherits []
      (List.filter_map
        (fun fstmnt ->
          match fstmnt with
          | pos, FLATBLOCKINHERIT (block, ref) -> Some (pos, block, ref)
          | _ -> None)
        fstmntls)
      in
  let result =
    List.fold_left
      (fun result (pos, block, ref) ->
          List.rev_append
            result
            (List.filter_map
            (fun (ns, stm) ->
              match stm with
              (* inheriting blockinherit is useless, due to the order we take
                  the inheritance of the inherited block is already resolved  
                  inheriting blockabstract is clearly wrong *)
              | FLATBLOCKINHERIT (_, _) -> None
              | FLATBLOCKABSTRACT -> None
              | _ -> (
                  match listminus ns block with
                  | None -> None
                  | Some ns'' -> (
                      match stm with
                      | FLATIFL (lbl', req') ->
                          Some (pos @ ns'', ifl_refine ns'' stm ref)
                      | _ -> Some (pos @ ns'', stm) )))
              result)
      )
      fstmntls
      inherits
  in
  List.sort_uniq compare result

(* let rec sort_calls lsorted lunsorted =
    (* an call is a pair of paths (path, c) such that 
       in the block at p there is a call c
       the desired partial order requires that a call c1 that is in the local
       environment of a macro called by c2 comes before c2.
       lsorted - calls sorted according to the partial order of calls
       lunsorted - non sorted calls
       returns a sorted list of the calls in the two lists *)
    let first, rest =
      List.partition
        (fun (pos, call) ->
          List.for_all
            (fun (pos2, call2) ->
              listminus pos2 block = None)
            lunsorted)
        lunsorted
      in
    if lunsorted = [] then lsorted else
      sort_inherits (List.rev_append (List.rev lsorted) first) rest *)


let phase3 fstmntls =
  (* returns a new IFCIL configuration where the names of macros in call statements are evaluated  *)
  print_string "phase 3\n";
  (* List.iter (fun i -> print_string (print_flat_CIL i ^ "\n")) fstmntls; *)
  (* flush stdout; *)
  let result =
    List.rev_map
      (fun fstmnt ->
        match fstmnt with
        | pos, FLATCALL (m, par, i) ->
            if pos = ["#"] || is_block pos fstmntls then
              try
              ( pos,
                FLATCALL (
                    eval_bl pos MACRO m fstmntls,
                    par,
                    i ) )
              with UndefinedReference s ->
                print_string "in phase3\n";
                raise (UndefinedReference s)
            else fstmnt
        | _ -> fstmnt)
      fstmntls
  in
  (* List.iter (fun i -> print_string (print_flat_CIL i ^ "\n")) result; *)
  List.sort_uniq compare result

let phase4' fstmntls (pos, FLATCALL (m, par, i)) =
  (* let fstmntls' =
    List.rev_append fstmntls
      (tl_flatten
         (List.rev_map
            (fun fstmnt ->
              match fstmnt with
              | ns, FLATCALL (m', par', i') ->
                  if ns = m then phase4' fstmntls (ns, FLATCALL (m', par', i'))
                  else []
              | _ -> [])
            fstmntls))
  in *)
  List.rev_map
    (fun (_, stm) -> (pos, stm))
    (List.filter
       (fun (pos', stmnt) ->
         match stmnt with
         | FLATTYPE t -> pos' = m
         | FLATATTRIBUTE t -> pos' = m
         | FLATTYPEALIAS t -> pos' = m
         (* | pos', FLATTYPEALIASACTUAL (t, t') -> pos' = m *)
         (* | pos', FLATATTRIBUTESET (n, expr) -> pos' = m *)
         | FLATCOMMON (name, perms) -> pos' = m
         (* | pos', FLATCLASSCOMMON (cls, com) -> pos' = m *)
         | FLATCLASS (cls, perms) -> pos' = m
         | FLATCLASSPERMISSION n -> pos' = m
         (* | pos', FLATCLASSPERMISSIONSET (clsperm, cls, perms) -> pos' = m *)
         | FLATCLASSMAP (clsmap, clsmappings) -> pos' = m
         (* | pos', FLATCLASSMAPPING (clsmap, clsmapping, clsperm) -> pos' = m *)
         | _ -> false)
       (* fstmntls') *)
       fstmntls)

let phase4 fstmntls =
  print_string "phase 4\n";
  flush stdout;
  let result =
    tl_flatten
      (List.rev_map
         (fun fstmnt ->
           match fstmnt with
           | pos, FLATCALL (m, par, i) ->
              if pos = ["#"] || is_block pos fstmntls then
                List.rev_append
                  (phase4' fstmntls (pos, FLATCALL (m, par, i)))
                  [ fstmnt ]
              else [ fstmnt ] 
           | _ -> [ fstmnt ])
         fstmntls)
  in
  List.sort_uniq compare result

let evaluate_local m stmnt fstmntls =
  (* 
    Evaluates the names occurring in the instructions stmnt inside the macro at m
    - if the names are defined inside the macro, then they left unhaltered,
    - otherwise, if they are defined in the local environment they are evaluated,
    - otherwise, they are unevaluated
  *)
  let resolve_name constr name = 
    eval_macr m constr name fstmntls
  in
  let resolve_kind kind =
    List.map
      (fun (n, a, n') -> (resolve_name TYPEorATTR n, a, resolve_name TYPEorATTR n'))
      kind
  in
  let rec resolve_attrexp exp =
    match exp with
    | A_NAME [ "all" ] -> exp
    | A_NAME name -> A_NAME (resolve_name TYPEorATTR name)
    | A_NOT e -> A_NOT (resolve_attrexp e)
    | A_AND (e1, e2) -> A_AND (resolve_attrexp e1, resolve_attrexp e2)
    | A_OR (e1, e2) -> A_OR (resolve_attrexp e1, resolve_attrexp e2)
    | A_XOR (e1, e2) -> A_XOR (resolve_attrexp e1, resolve_attrexp e2)
  in
  let resolve_classperm classperms =
    (* Only non trivial part, because synthax does not tell how it is defined :*( *)
    (
    let _, localns = last_and_list m in  
    match classperms with
    | Name n -> Name (resolve_name CLASSPERMISSION n)
    | Anonym (cls, permcon) -> (
        (* may be classmapping or class and classpermcon *)
          if (eval_scope m CLASSMAP cls fstmntls != None) ||
             (eval_scope m CLASS cls fstmntls != None) then classperms
          else  
            try (
          match eval_local localns CLASSMAP cls fstmntls with
            | Some evaluated_anonym -> Anonym (evaluated_anonym, permcon)
            | None -> 
                (let rescls = resolve_name CLASS cls
                and resperms =
                  (* match permcon with
                    | Permissions perms -> permcon
                    | Expression exp -> Expression (resolve_classpermcon exp) *)
                  permcon
                in
                Anonym (rescls, resperms))
                )
              with UndefinedReference s -> 
                print_string ("in resolve_classperm for (ignora allow) "
                  ^ (print_flat_CIL (["#"], FLATALLOW (["#"], ["#"], classperms))));
                raise (UndefinedReference s)
                ))
  in
  match stmnt with
  | FLATALLOW (src, trg, clasperms) ->
      FLATALLOW
          ( resolve_name TYPEorATTR src,
            resolve_name TYPEorATTR trg,
            resolve_classperm clasperms )
  | FLATATTRIBUTESET (attr, exp) ->
      FLATATTRIBUTESET (resolve_name ATTRIBUTE attr, resolve_attrexp exp)
  | FLATIFL (lbl, MUST kind) -> FLATIFL (lbl, MUST (resolve_kind kind))
  | FLATIFL (lbl, MUSTNOT kind) ->
      FLATIFL (lbl, MUSTNOT (resolve_kind kind))
  | FLATIFL (lbl, EVERYMUST (kind, kin')) ->
      FLATIFL (lbl, EVERYMUST (resolve_kind kind, resolve_kind kin'))
  | FLATTYPEALIASACTUAL (alias, actual) ->
      (* typealias is for types, not for attributes *)
      FLATTYPEALIASACTUAL (alias, resolve_name TYPE actual)
  | FLATCLASSCOMMON (cls, cmn) ->
      FLATCLASSCOMMON (resolve_name CLASS cls, resolve_name COMMON cmn)
  | FLATCLASSPERMISSIONSET (clsperms, cls, perms) ->
      FLATCLASSPERMISSIONSET
          (resolve_name CLASSPERMISSION clsperms, resolve_name CLASS cls, perms)
  | FLATCLASSMAPPING (clsmap, clasmapi, cperms) ->
        FLATCLASSMAPPING
          (resolve_name CLASSMAP clsmap, clasmapi, resolve_classperm cperms)
  | FLATCALL (mcr, par, ref) ->
    (* TODO. resolve par and ref *)
        FLATCALL (resolve_name MACRO mcr, par, ref)
  | _ -> raise (UnsupportedConstruct "locally evaluation something unexpected")

let rec phase5' fstmntls (pos, FLATCALL (m, par, i)) =
  (* 
     manme - is the name of the macro, localns is its path 
     localns - is the path of the macro
     mpath - is the path of the block where the macro is defined
     mcr - is the macro definition statement
  *)
  let mname, localns = last_and_list m in
  let mpath, mcr =
    try
      List.find
        (fun fstm ->
          match fstm with
          | pos'', FLATMACRO (m', _) -> m' = mname && pos'' = localns
          | _ -> false)
        fstmntls
    with Not_found ->
      print_string
        (mname ^ " " ^ String.concat "." localns ^ " " ^ String.concat "." m);
      raise Not_found
  in
  (* TODO add evaluation for class parameters etc *)
  (* a_f_pairs is a list of pairs (actual parameters, formal parameters) *)
  let a_f_pars =
    match mcr with
    | FLATMACRO (_, fpar) -> List.combine par fpar
    | _ -> raise Not_found
  in
  (* evalpar', dato il nome di un parametro attuale lo valuta secondo 
     il tipo (type, attribute, class etc) che gli assegna il parametro formale *)
  let evalpar' name =
    match List.find_opt (fun (a, f) -> a = name) a_f_pars with
    | None -> name
    | Some (a, f) -> (
        match f with
        | PARTYPE, _ ->
            eval_bl pos TYPEorATTR name fstmntls 
        | PARCLASS, _ -> eval_bl pos CLASS name fstmntls
        | PARTYPEALIAS, _ ->
            raise
              (UnsupportedConstruct "typealias parameters are not supported")
        | PARCLASSPERMISSION, _ -> eval_bl pos CLASSPERMISSION name fstmntls
        | PARCLASSMAP, _ -> eval_bl pos CLASSMAP name fstmntls
        | PARIGNORE, _ -> name)
  in
  let i' =
    List.map
      (fun (lbl, lbl', req) ->
        ( lbl,
          lbl',
          match req with
          | MUST kind ->
              MUST
                (List.map (fun (n, a, n') -> (evalpar' n, a, evalpar' n')) kind)
          | MUSTNOT kind ->
              MUSTNOT
                (List.map (fun (n, a, n') -> (evalpar' n, a, evalpar' n')) kind)
          | EVERYMUST (kind, kind') ->
              EVERYMUST
                ( List.map (fun (n, a, n') -> (evalpar' n, a, evalpar' n')) kind,
                  List.map
                    (fun (n, a, n') -> (evalpar' n, a, evalpar' n'))
                    kind' ) ))
      i
  in
  match (mpath, mcr) with
  | pos'', FLATMACRO (m', fpar) ->
      let apply_params name =
        match List.find_opt (fun (a, (_, f)) -> [ f ] = name) a_f_pars with
        | Some (a, _) -> evalpar' a
        | None -> name
      in
      let rec apply_params_attrset exp =
        match exp with
        | A_NAME [ "all" ] -> exp
        | A_NAME name -> A_NAME (apply_params name)
        | A_NOT e -> A_NOT (apply_params_attrset e)
        | A_AND (e1, e2) ->
            A_AND (apply_params_attrset e1, apply_params_attrset e2)
        | A_OR (e1, e2) ->
            A_OR (apply_params_attrset e1, apply_params_attrset e2)
        | A_XOR (e1, e2) ->
            A_XOR (apply_params_attrset e1, apply_params_attrset e2)
      and apply_params_kind kind =
        List.map
          (fun (n, marrow, n') -> (apply_params n, marrow, apply_params n'))
          kind
      and apply_params_classperms classperms =
        match classperms with
        | Name id -> Name (apply_params id)
        | Anonym (cls, perms) -> Anonym (apply_params cls, perms)
      in
      let apply_params_iflreq req =
            match req with
            | MUST kind -> MUST (apply_params_kind kind)
            | MUSTNOT kind -> MUSTNOT (apply_params_kind kind)
            | EVERYMUST (kind, kind') ->
                EVERYMUST
                  (apply_params_kind kind, apply_params_kind kind')
      in
      (* nsstmntls - list of statements that are inside the macro where
          in the statement the parameters are applied  *)
      let nsstmntls =
        (* print_string ("\nevaluating call of macro " ^ (String.concat "." m) ^ "\n"); *)
        List.filter_map
          (fun (pos, ins) -> 
            (* print_string ("considering instr in path " ^ (String.concat "." pos) ^ "\n"); *)
            if pos <> m then None else 
            (
              (* print_string "siamo in m\n";
            flush; *)
            match ins with
              | FLATALLOW (src, trg, clasperms) ->
                let vsrc = apply_params src
                and vtrg = apply_params trg
                and vclasperms = apply_params_classperms clasperms in
                  Some (FLATALLOW (vsrc, vtrg, vclasperms))
              | FLATATTRIBUTESET (n, e) ->
                    Some (FLATATTRIBUTESET (n, apply_params_attrset e))
                    (*
                      | FLATCLASSMAPPING of string list * string list * classpermission *)
              | FLATTYPEALIASACTUAL (n, qn) ->
                  Some (FLATTYPEALIASACTUAL (n, apply_params qn))
              | FLATCLASSCOMMON (cls, cmn) ->
                  (try
                    Some (FLATCLASSCOMMON (apply_params cls, cmn))
                  with UndefinedReference s ->
                    print_string
                      ("in flatclasscommon -- ");
                    raise (UndefinedReference s))
              | FLATCLASSPERMISSIONSET (id, cls, expr) ->
                (try
                  Some (FLATCLASSPERMISSIONSET (id, apply_params cls, expr))
                with UndefinedReference s ->
                  print_string
                    ("in flatclasscommon -- ");
                  raise (UndefinedReference s))
              | FLATCLASSMAPPING (cmap, cmapi, perms) ->
                    (* TODO: ppply params, is it right? *)
                    Some (FLATCLASSMAPPING (apply_params cmap, cmapi, perms))
              | FLATIFL (lbl, req) ->
                     Some (FLATIFL (lbl, apply_params_iflreq req))
              | FLATCALL (m', par', i') ->
                    let vpar' = List.map (fun p -> apply_params p) par' in
                    let vi' = List.map (fun (lbl, oldlbl, req) -> (lbl, oldlbl, apply_params_iflreq req)) i' in
                    Some (FLATCALL (m', vpar', vi'))
              | FLATTYPE _
              | FLATTYPEALIAS _
              | FLATATTRIBUTE _
              | FLATCLASS (_, _)
              | FLATCLASSPERMISSION _
              | FLATCOMMON (_, _)
              | FLATCLASSMAP (_, _) -> None
              | FLATMACRO (_) 
              | FLATBLOCKABSTRACT
              | FLATBLOCK _
              | FLATBLOCKINHERIT (_, _) -> raise (OurError "unexpected construscts inside macro")))
          fstmntls
      in
      List.rev_map
        (fun stm ->
          match stm with
          | FLATALLOW (_, _, _)
          | FLATATTRIBUTESET (_, _)
          | FLATCLASSCOMMON (_, _)
          | FLATCLASSPERMISSIONSET (_, _, _)
          | FLATCLASSMAPPING (_, _, _)
          | FLATCALL (_, _, _)
          | FLATTYPEALIASACTUAL (_, _) ->
              (pos, evaluate_local m stm fstmntls)
          | FLATIFL (lbl, req) ->
              let req' = evaluate_local m stm fstmntls in
              (pos, ifl_refine [] req' i')
                (* TODO locally evaluate par and ref *)
          | FLATTYPE _
          | FLATTYPEALIAS _
          | FLATATTRIBUTE _
          | FLATCLASS (_, _)
          | FLATCLASSPERMISSION _
          | FLATCOMMON (_, _)
          | FLATCLASSMAP (_, _) -> raise (OurError "declaration inside macro should be removed")
          | FLATMACRO (_) 
          | FLATBLOCKABSTRACT
          | FLATBLOCK _
          | FLATBLOCKINHERIT (_, _) -> raise (OurError "unexpected construscts inside macro")
        )
        nsstmntls
  | _ -> raise (OurError "unexpected error2")

let phase5 fstmntls =
  print_string "phase 5\n";
  flush stdout;
  let result =
    List.fold_left
      (fun nfstmnts fstmnt ->
        match fstmnt with
        | pos, FLATCALL (m, par, i) -> (
            if pos = ["#"] || is_block pos fstmntls then
              (try
                List.rev_append nfstmnts
                    (phase5' fstmntls (pos, FLATCALL (m, par, i)))
                    (* ???? *)
              with UndefinedReference s -> 
                print_string (s ^ " -- nooo\n"); nfstmnts)
                (* nooo happens in opnWRT! *)
            else fstmnt :: nfstmnts)
        | _ -> fstmnt :: nfstmnts)
      [] fstmntls
  in
  List.sort_uniq compare result

  let rec phase31 fstmntls = 
  (* returns a new IFCIL configuration where the macro calls are resolved *)
    print_string "phase 31\n";
    (* print_string "phase 31:: \n";
    List.iter (fun i -> print_string (print_flat_CIL i ^ "\n")) fstmntls; *)
    if 
      List.exists
        (fun fstmnt ->
          match fstmnt with
          | pos, FLATCALL (m, par, i) ->
              pos = ["#"] || is_block pos fstmntls
          | _ -> false)
        fstmntls
    then 
      (
        print_string "phase 31 -- 3,4,5\n";
        phase31 
          ((phase5 << phase4 << phase3) fstmntls)
      )
    else
      fstmntls
  

let phase6 fstmntls =
  (* returns a new IFCIL configuration where all the names are resolved *)
  print_string "phase 6\n";
  flush stdout;
  let eval pos name =
    try
      if name = [ "any-node" ] then name
      else eval_bl pos TYPEorATTR name fstmntls
    with UndefinedReference _ -> eval_bl [ "#" ] TYPEorATTR name fstmntls
  in
  let eval_attr pos name =
    try eval_bl pos ATTRIBUTE name fstmntls
    with UndefinedReference _ ->
      eval_bl [ "#" ] ATTRIBUTE name fstmntls
  in
  let eval_kind k pos =
    List.map (fun (n, a, n') -> (eval pos n, a, eval pos n')) k
  in
  let rec eval_attrexp exp pos =
    match exp with
    | A_NAME [ "all" ] -> exp
    | A_NAME name -> A_NAME (eval pos name)
    | A_NOT e -> A_NOT (eval_attrexp e pos)
    | A_AND (e1, e2) -> A_AND (eval_attrexp e1 pos, eval_attrexp e2 pos)
    | A_OR (e1, e2) -> A_OR (eval_attrexp e1 pos, eval_attrexp e2 pos)
    | A_XOR (e1, e2) -> A_XOR (eval_attrexp e1 pos, eval_attrexp e2 pos)
  in
  let eval_class name pos =
    try eval_bl pos CLASS name fstmntls
    with UndefinedReference _ -> eval_bl [ "#" ] CLASS name fstmntls
  in
  let eval_common name pos =
    try eval_bl pos COMMON name fstmntls
    with UndefinedReference _ -> eval_bl [ "#" ] COMMON name fstmntls
  in
  let eval_classmap name pos =
    try eval_bl pos CLASSMAP name fstmntls
    with UndefinedReference _ -> eval_bl [ "#" ] CLASSMAP name fstmntls
  in
  let eval_class_permission name pos =
    (* List.iter
       (fun fstmnt ->
         print_string (print_flat_CIL fstmnt ^ "\n"))
       fstmntls; *)
    try eval_bl pos CLASSPERMISSION name fstmntls
    with UndefinedReference _ -> eval_bl [ "#" ] CLASSPERMISSION name fstmntls
  in
  let eval_classperm e pos =
    match e with
    | Name n -> Name (eval_class_permission n pos)
    | Anonym (cls, perms) -> (
        try Anonym (eval_class cls pos, perms)
        with UndefinedReference _ -> Anonym (eval_classmap cls pos, perms))
    (* Here perms are the classmapping, hence a name that cannot be evaluated, in classmapping statements the classmap is evaluated *)
  in
  let result =
    List.fold_left
      (fun fstmntls' (pos, fstmnt) ->
        if
          pos = [ "#" ]
          || is_block pos fstmntls
            && not
                  (List.exists
                    (fun (pos', stm) ->
                      list_start_with pos pos' && stm = FLATBLOCKABSTRACT)
                    fstmntls)
        then
          (match fstmnt with
          | FLATALLOW (src, trg, clasperms) -> (
              let vsrc = eval pos src
              and vtrg = eval pos trg
              and vclasperms = eval_classperm clasperms pos in
              try (pos, FLATALLOW (vsrc, vtrg, vclasperms))
              with UndefinedReference s ->
                raise
                  (UndefinedReference
                    (s ^ " -- flatallow " ^ "in " ^ String.concat "." pos ^ " "
                    ^ String.concat "." src ^ " " ^ String.concat "." trg
                    ^ print_classpermission clasperms)))
          | FLATATTRIBUTESET (a, e) -> (
              try (pos, FLATATTRIBUTESET (eval_attr pos a, eval_attrexp e pos))
              with UndefinedReference s ->
                raise
                  (UndefinedReference
                    (s ^ " -- flatattributeset " ^ "in " ^ String.concat "." pos
                    ^ " " ^ String.concat "." a ^ " " ^ print_attrset e)))
          | FLATTYPEALIASACTUAL (n, qn) -> (
              try (pos, FLATTYPEALIASACTUAL (n, eval pos qn))
              with UndefinedReference s ->
                raise
                  (UndefinedReference
                    (s ^ " -- flattypeasctual " ^ "in " ^ String.concat "." pos
                    ^ " " ^ n ^ " " ^ String.concat "." qn)))
          | FLATCLASSCOMMON (cls, cmn) -> (
              try (pos, FLATCLASSCOMMON (eval_class cls pos, eval_common cmn pos))
              with UndefinedReference s ->
                raise
                  (UndefinedReference
                    (s ^ " -- flatclasscommon " ^ "in " ^ String.concat "." pos
                    ^ " " ^ String.concat "." cls ^ " " ^ String.concat "." cmn)))
          | FLATCLASSPERMISSIONSET (id, cls, expr) -> (
              try
                ( pos,
                  FLATCLASSPERMISSIONSET
                    (eval_class_permission id pos, eval_class cls pos, expr) )
              with UndefinedReference s ->
                raise
                  (UndefinedReference
                    (s ^ " -- flatclasspermissionset " ^ "in "
                    ^ String.concat "." pos ^ " " ^ String.concat "." id ^ " "
                    ^ String.concat "." cls ^ " "
                    ^ print_classpermissionsetcon expr)))
          | FLATIFL (lbl, MUST kind) ->
              (pos, FLATIFL (lbl, MUST (eval_kind kind pos)))
          | FLATIFL (lbl, MUSTNOT kind) ->
              (pos, FLATIFL (lbl, MUSTNOT (eval_kind kind pos)))
          | FLATIFL (lbl, EVERYMUST (kind, kind')) ->
              ( pos,
                FLATIFL (lbl, EVERYMUST (eval_kind kind pos, eval_kind kind' pos))
              )
          | FLATCLASSMAPPING (cmap, cmapping, perms) -> (
              try
                match perms with
                | Name clsperms ->
                    ( pos,
                      FLATCLASSMAPPING
                        ( eval_classmap cmap pos,
                          cmapping,
                          Name (eval_class_permission clsperms pos) ) )
                | Anonym (cls, pms) -> (
                    ( pos,
                      try
                        FLATCLASSMAPPING
                          ( eval_classmap cmap pos,
                            cmapping,
                            Anonym (eval_class cls pos, pms) )
                      with UndefinedReference _ ->
                        FLATCLASSMAPPING
                          ( eval_classmap cmap pos,
                            cmapping,
                            Anonym (eval_classmap cls pos, pms) ) ))
              with UndefinedReference s ->
                raise
                  (UndefinedReference
                    (s ^ " -- flatclassmapping " ^ "in " ^ String.concat "." pos
                    ^ " " ^ String.concat "." cmap ^ " "
                    ^ String.concat "." cmapping ^ " "
                    ^ print_classpermission perms)))
          | _ -> (pos, fstmnt))
          :: fstmntls'
        else fstmntls')
      [] fstmntls
  in
  (* List.sort_uniq compare  *)
  (* List.iter (fun i -> print_string (print_flat_CIL i ^ "\n")) result; *)
  result

let normalize fstmntls =
  let result =
    (phase6 << phase31 << phase2 << phase1) fstmntls
  in
  print_string "normalization completed\n";
  (* List.iter (fun i -> print_string (print_flat_CIL i ^ "\n")) result; *)
  result

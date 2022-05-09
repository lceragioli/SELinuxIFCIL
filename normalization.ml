open CILsyntax
open IFCILconfiguration
open Utils
open IFL
module SS = Set.Make (String)

exception UndefinedMeet of string

exception NotUniqueMeet of string

exception UncorrectRefinement of string

let ifl_refine ns (FLATIFL (lbl, req)) i =
  try
    match
      List.hd (List.filter (fun (lbl'', lbl', req') -> lbl' = ns @ [ lbl ]) i)
    with
    | lbl'', _, req' ->
        if minor req' req then FLATIFL (lbl'', req')
        else raise (UncorrectRefinement "")
  with Failure s -> if s = "hd" then FLATIFL (lbl, req) else raise (Failure s)

let phase1 ifcilconfig =
  print_string "phase 1\n";
  flush stdout;

  let result =
    SLM.mapi
      (fun pos costrls->
        List.rev_map
        (fun fstmnt ->
          match fstmnt with
          | FLATBLOCKINHERIT (b, i) ->
              FLATBLOCKINHERIT
                  ( (try eval_bl pos BLOCK b ifcilconfig
                     with UndefinedReference _ ->
                       eval_bl [ "#" ] BLOCK b ifcilconfig),
                    i )
          | fstmt -> fstmt)
        costrls
      )
    ifcilconfig
  in
  SLM.iter (
    fun path cons ->
      (List.iter
        (fun j ->  print_string (print_path path ^ (print_flat_CIL j ^ "\n")))
        cons))
    result;
  result

let rec phase2' ifcilconfig (pos, FLATBLOCKINHERIT (b, i)) =

  let inherited_ifcilconfigs =
    SLM.fold
      (fun pos' costrls ls ->
          if listminus pos' b != None then
            List.rev_append
            (
              List.filter_map
                (fun fstmnt ->
                  match fstmnt with
                  | FLATBLOCKINHERIT (b', i') ->
                      Some (phase2' ifcilconfig (pos', FLATBLOCKINHERIT (b', i')))
                  | _ -> None)
                costrls
            )
            ls
          else ls
      )
      ifcilconfig
      []
  in
  let union_conf =
    List.fold_left
      (SLM.union 
          (fun _ ls1 ls2 -> Some (List.rev_append ls1 ls2))
      )
      ifcilconfig
      inherited_ifcilconfigs
  in
  SLM.fold
    (fun pos' fstmls conf ->
      match listminus pos' b with
      | None -> conf
      | Some pos'' -> 
          SLM.update
            (pos @ pos'')
            (fun old ->
              let news = 
                List.map
                  (fun stm ->
                    match stm with
                      | FLATIFL (lbl', req') ->
                          ifl_refine pos'' stm i
                      | _ -> stm
                    )
                  fstmls
              in
              match old with
              | None -> Some news
              | Some oldls -> Some (List.rev_append oldls news)
              )
              conf)
    union_conf
    SLM.empty

let phase2 ifcilconfig =
  print_string "phase 2\n";
  flush stdout;
  let inherited_ifcilconfigs =
    SLM.fold
      (fun pos costrls ls ->
        if List.exists
            (fun fstm ->
              fstm = FLATBLOCKABSTRACT)
            costrls
          then ls else
            List.rev_append
            (
              List.filter_map
                (fun fstmnt ->
                  match fstmnt with
                  | FLATBLOCKINHERIT (b, i) ->
                            Some (phase2' ifcilconfig (pos, FLATBLOCKINHERIT (b, i)))
                  | _ -> None)
                costrls
            )
            ls
      )
      ifcilconfig
      []
    in
  let result =
    SLM.filter
      (fun _ fstmls ->
        not 
          (List.exists
            (fun fstm -> fstm = FLATBLOCKABSTRACT)
            fstmls))
      (List.fold_left
        (SLM.union 
            (fun _ ls1 ls2 -> Some (List.rev_append ls1 ls2))
        )
        ifcilconfig
        inherited_ifcilconfigs)
  in
  SLM.iter (
    fun path cons ->
      (List.iter
        (fun j ->  print_string (print_path path ^ (print_flat_CIL j ^ "\n")))
        cons))
    result;
  result

let phase3 ifcilconfig =
  print_string "phase 3\n";
  flush stdout;

  let result =
    SLM.mapi
      (fun pos costrls->
        List.rev_map
        (fun fstmnt ->
          match fstmnt with
          | FLATCALL (m, par, i) ->
              FLATCALL
                  ( (try eval_bl pos MACRO m ifcilconfig
                     with UndefinedReference _ ->
                       eval_bl [ "#" ] MACRO m ifcilconfig),
                    par,
                    i )
          | fstmt -> fstmt)
        costrls
      )
    ifcilconfig
  in
  SLM.iter (
    fun path cons ->
      (List.iter
        (fun j ->  print_string (print_path path ^ (print_flat_CIL j ^ "\n")))
        cons))
    result;
  result

(* 
let rec phase4' fstmntls (pos, FLATCALL (m, par, i)) =
  let fstmntls' =
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
  in
  List.rev_map
    (fun (_, stm) -> (pos, stm))
    (List.filter
       (fun stmnt ->
         match stmnt with
         | pos', FLATTYPE t -> pos' = m
         | pos', FLATATTRIBUTE t -> pos' = m
         | pos', FLATTYPEALIAS t -> pos' = m
         (* | pos', FLATTYPEALIASACTUAL (t, t') -> pos' = m *)
         (* | pos', FLATATTRIBUTESET (n, expr) -> pos' = m *)
         | pos', FLATCOMMON (name, perms) -> pos' = m
         (* | pos', FLATCLASSCOMMON (cls, com) -> pos' = m *)
         | pos', FLATCLASS (cls, perms) -> pos' = m
         | pos', FLATCLASSPERMISSION n -> pos' = m
         (* | pos', FLATCLASSPERMISSIONSET (clsperm, cls, perms) -> pos' = m *)
         | pos', FLATCLASSMAP (clsmap, clsmappings) -> pos' = m
         (* | pos', FLATCLASSMAPPING (clsmap, clsmapping, clsperm) -> pos' = m *)
         | _ -> false)
       fstmntls')

let phase4 fstmntls =
  print_string "phase 4\n";
  flush stdout;
  let result =
    tl_flatten
      (List.rev_map
         (fun fstmnt ->
           match fstmnt with
           | pos, FLATCALL (m, par, i) ->
               List.rev_append
                 (phase4' fstmntls (pos, FLATCALL (m, par, i)))
                 [ fstmnt ]
           | _ -> [ fstmnt ])
         fstmntls)
  in
  result

let evaluate_local m pos' pos stmnt fstmntls =
  let _, localns = last_and_list m in
  let resolve_att name =
    try if eval_macr m ATTRIBUTE name fstmntls != [] then name else name
    with UndefinedReference _ -> (
      try eval_bl localns ATTRIBUTE name fstmntls
      with UndefinedReference _ -> name)
  in
  let resolve_type name =
    try
      if eval_macr m TYPE name fstmntls != [] then name
      else name (* May create problems if types and attributes share names *)
    with UndefinedReference _ -> (
      try eval_bl localns TYPE name fstmntls with UndefinedReference _ -> name)
  in
  let resolve_common name =
    try
      if eval_macr m COMMON name fstmntls != [] then name
      else name (* May create problems if types and attributes share names *)
    with UndefinedReference _ -> (
      try eval_bl localns COMMON name fstmntls
      with UndefinedReference _ -> name)
  in
  let resolve_type_or_att name =
    try
      if eval_macr m TYPE name fstmntls != [] then name
      else name (* May create problems if types and attributes share names *)
    with UndefinedReference _ -> (
      try if eval_macr m ATTRIBUTE name fstmntls != [] then name else name
      with UndefinedReference _ -> (
        try eval_bl localns TYPE name fstmntls
        with UndefinedReference _ -> (
          try eval_bl localns ATTRIBUTE name fstmntls
          with UndefinedReference _ -> name)))
  in
  let resolve_kind kind =
    List.map
      (fun (n, a, n') -> (resolve_type_or_att n, a, resolve_type_or_att n'))
      kind
  in
  let rec resolve_attrexp exp =
    match exp with
    | A_NAME [ "all" ] -> exp
    | A_NAME name -> A_NAME (resolve_type_or_att name)
    | A_NOT e -> A_NOT (resolve_attrexp e)
    | A_AND (e1, e2) -> A_AND (resolve_attrexp e1, resolve_attrexp e2)
    | A_OR (e1, e2) -> A_OR (resolve_attrexp e1, resolve_attrexp e2)
    | A_XOR (e1, e2) -> A_XOR (resolve_attrexp e1, resolve_attrexp e2)
  and resolve_class cls =
    try if eval_macr m CLASS cls fstmntls != [] then cls else cls
    with UndefinedReference _ -> (
      try eval_bl localns CLASS cls fstmntls with UndefinedReference _ -> cls)
  and resolve_classmap cls =
    try if eval_macr m CLASSMAP cls fstmntls != [] then cls else cls
    with UndefinedReference _ -> (
      try eval_bl localns CLASSMAP cls fstmntls
      with UndefinedReference _ -> cls)
  and resolve_classpermname name =
    try if eval_macr m CLASSPERMISSION name fstmntls != [] then name else name
    with UndefinedReference _ -> (
      try eval_bl localns CLASSPERMISSION name fstmntls
      with UndefinedReference _ -> name)
  in
  let resolve_classperm classperms =
    match classperms with
    | Name n -> Name (resolve_classpermname n)
    | Anonym (cls, permcon) -> (
        (* may be classmapping or class and classpermcon *)
        try
          if eval_macr m CLASSMAP cls fstmntls != [] then classperms
          else classperms
        with UndefinedReference _ -> (
          try Anonym (eval_bl localns CLASSMAP cls fstmntls, permcon)
          with UndefinedReference _ ->
            let rescls = resolve_class cls
            and resperms =
              (* match permcon with
                 | Permissions perms -> permcon
                 | Expression exp -> Expression (resolve_classpermcon exp) *)
              permcon
            in
            Anonym (rescls, resperms)))
  in
  match stmnt with
  | FLATALLOW (src, trg, clasperms) ->
      ( pos,
        FLATALLOW
          ( resolve_type_or_att src,
            resolve_type_or_att trg,
            resolve_classperm clasperms ) )
  | FLATATTRIBUTESET (attr, exp) ->
      (pos, FLATATTRIBUTESET (resolve_att attr, resolve_attrexp exp))
  | FLATIFL (lbl, MUST kind) -> (pos, FLATIFL (lbl, MUST (resolve_kind kind)))
  | FLATIFL (lbl, MUSTNOT kind) ->
      (pos, FLATIFL (lbl, MUSTNOT (resolve_kind kind)))
  | FLATIFL (lbl, EVERYMUST (kind, kin')) ->
      (pos, FLATIFL (lbl, EVERYMUST (resolve_kind kind, resolve_kind kin')))
  | FLATTYPEALIASACTUAL (alias, actual) ->
      (* typealias is for types, not for attributes *)
      (pos, FLATTYPEALIASACTUAL (alias, resolve_type actual))
  | FLATCLASSCOMMON (cls, cmn) ->
      (pos, FLATCLASSCOMMON (resolve_class cls, resolve_common cmn))
  | FLATCLASSPERMISSIONSET (clsperms, cls, perms) ->
      ( pos,
        FLATCLASSPERMISSIONSET
          (resolve_classpermname clsperms, resolve_class cls, perms) )
  | FLATCLASSMAPPING (clsmap, clasmapi, cperms) ->
      ( pos,
        FLATCLASSMAPPING
          (resolve_classmap clsmap, clasmapi, resolve_classperm cperms) )
  | _ -> raise (UnsupportedConstruct "locally evaluation something unexpected")

let rec phase5' fstmntls (pos, FLATCALL (m, par, i)) =
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
  let a_f_pars =
    match mcr with
    | FLATMACRO (_, fpar) -> List.combine par fpar
    | _ -> raise Not_found
  in
  let evalpar' name =
    match List.find_opt (fun (a, f) -> a = name) a_f_pars with
    | None -> name
    | Some (a, f) -> (
        match f with
        | PARTYPE, _ -> (
            try eval_bl pos TYPE name fstmntls
            with UndefinedReference _ -> (
              try eval_bl pos ATTRIBUTE name fstmntls
              with UndefinedReference _ -> (
                try eval_bl [ "#" ] TYPE name fstmntls
                with UndefinedReference _ ->
                  eval_bl [ "#" ] ATTRIBUTE name fstmntls)))
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
      let nsstmntls =
        List.rev_map
          (fun stm ->
            match stm with
            | p''', FLATALLOW (src, trg, clasperms) ->
                if p''' = m then
                  let vsrc = apply_params src
                  and vtrg = apply_params trg
                  and vclasperms = apply_params_classperms clasperms in
                  (p''', FLATALLOW (vsrc, vtrg, vclasperms))
                else stm
            | p''', FLATATTRIBUTESET (n, e) ->
                if p''' = m then
                  (p''', FLATATTRIBUTESET (n, apply_params_attrset e))
                else stm
                  (*
                     | FLATCLASSMAPPING of string list * string list * classpermission *)
            | p''', FLATTYPEALIASACTUAL (n, qn) ->
                if p''' = m then (p''', FLATTYPEALIASACTUAL (n, apply_params qn))
                else stm
            | p''', FLATCLASSCOMMON (cls, cmn) ->
                if p''' = m then (p''', FLATCLASSCOMMON (apply_params cls, cmn))
                else stm
            | p''', FLATCLASSPERMISSIONSET (id, cls, expr) ->
                if p''' = m then
                  (p''', FLATCLASSPERMISSIONSET (id, apply_params cls, expr))
                else stm
            | p''', FLATCLASSMAPPING (cmap, cmapi, perms) ->
                if p''' = m then
                  (* TODO: ppply params *)
                  (p''', FLATCLASSMAPPING (cmap, cmapi, perms))
                else stm
            | p''', FLATIFL (lbl, req) ->
                if p''' = m then
                  let vreq =
                    match req with
                    | MUST kind -> MUST (apply_params_kind kind)
                    | MUSTNOT kind -> MUSTNOT (apply_params_kind kind)
                    | EVERYMUST (kind, kind') ->
                        EVERYMUST
                          (apply_params_kind kind, apply_params_kind kind')
                  in
                  (p''', FLATIFL (lbl, vreq))
                else stm
            | p''', FLATCALL (m', par', i') ->
                if p''' = m then
                  let vpar' = List.map (fun p -> apply_params p) par' in
                  (p''', FLATCALL (m', vpar', i'))
                else stm
            | _ -> stm)
          fstmntls
      in
      List.rev_map
        (fun (pos', stm) ->
          match stm with
          | FLATALLOW (_, _, _)
          | FLATATTRIBUTESET (_, _)
          | FLATCLASSCOMMON (_, _)
          | FLATCLASSPERMISSIONSET (_, _, _)
          | FLATCLASSMAPPING (_, _, _)
          | FLATTYPEALIASACTUAL (_, _) ->
              evaluate_local m pos' pos stm nsstmntls
          | FLATIFL (lbl, req) ->
              let pos'', req' = evaluate_local m pos' pos stm nsstmntls in
              (pos'', ifl_refine [] req' i')
          | _ -> raise (OurError "unexplicable"))
        (List.rev_append
           (List.filter
              (fun stmnt ->
                match stmnt with
                | pos, FLATALLOW (_, _, _)
                | pos, FLATATTRIBUTESET (_, _)
                | pos, FLATCLASSCOMMON (_, _)
                | pos, FLATCLASSPERMISSIONSET (_, _, _)
                | pos, FLATCLASSMAPPING (_, _, _)
                | pos, FLATTYPEALIASACTUAL (_, _)
                | pos, FLATIFL (_, _) ->
                    pos = m
                | _ -> false)
              nsstmntls)
           (tl_flatten
              (List.rev_map
                 (fun (_, stm) -> phase5' nsstmntls (pos, stm))
                 (List.filter
                    (fun stmnt ->
                      match stmnt with
                      | pos, FLATCALL (m', par', i') -> pos = m
                      | _ -> false)
                    nsstmntls))))
  | _ -> raise (OurError "unexplicable")

let phase5 fstmntls =
  print_string "phase 5\n";
  flush stdout;
  let result =
    List.fold_left
      (fun nfstmnts fstmnt ->
        match fstmnt with
        | pos, FLATCALL (m, par, i) -> (
            let b, pos' = last_and_list pos in
            try
              if eval_bl pos' BLOCK [ b ] fstmntls != [] then
                List.rev_append nfstmnts
                  (phase5' fstmntls (pos, FLATCALL (m, par, i)))
              else
                List.rev_append nfstmnts
                  (phase5' fstmntls (pos, FLATCALL (m, par, i)))
            with UndefinedReference _ -> fstmnt :: nfstmnts)
        | _ -> fstmnt :: nfstmnts)
      [] fstmntls
  in
  result

let phase6 fstmntls =
  print_string "phase 6\n";
  flush stdout;
  let eval pos name =
    try
      if name = [ "any-node" ] then name
      else eval_bl_type_attr pos TYPE name fstmntls
    with UndefinedReference _ -> eval_bl_type_attr [ "#" ] TYPE name fstmntls
  in
  let eval_attr pos name =
    try eval_bl_type_attr pos ATTRIBUTE name fstmntls
    with UndefinedReference _ ->
      eval_bl_type_attr [ "#" ] ATTRIBUTE name fstmntls
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
        (* print_string "ciao\n"; *)
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
            (* print_string ("ciao" ^ (String.concat "." pos) ^ (print_IFL_requirement (MUST kind))); *)
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
    [] fstmntls *)

let normalize ifcilconfig =
  let result =
    (* (phase6 << phase5 << phase4 << phase3 << phase2 << phase1) fstmntls *)
    (phase3 << phase2 << phase1) ifcilconfig
  in
  print_string "normalization completed\n";
  result

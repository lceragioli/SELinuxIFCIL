open IFCILsyntax
open IFCILsyntaxE
open CILenvE
open Utils
open CILclass

type semantics =
  {
    nodes: qn list;
    allows: (torat_node * SS.t * torat_node) list;
    ta: (qn * attributeexpE) list;
    ifl: iflreqE SM.t;
    bigo: SS.t
  }

let t1' rho ns rule =
  match rule with
  | CILBLOCKINHERIT qn ->
      (match eval_b_bar qn rho [ns] with
      | Some (fn, _) -> CILBLOCKINHERIT fn
      | None -> failwith ("undefined block to inherit " ^ qn_tostring qn))
  | _ -> rule

let t1 cmd rho = 
  SLM.mapi
    (fun ns blck -> 
      { (blck:Block.t) with 
        rules = 
          (List.rev_map
            (t1' rho ns) 
            blck.rules )})
    cmd

let rec t2'' cmd rho ns rule resrho = 
  match rule with 
  | CILBLOCKINHERIT qn -> 
    (match eval_b_bar qn rho [] with
      | Some (_, ns') ->
          let rho' = t2' cmd rho qn rho in
          rho_plus_rho_m ns qn resrho rho'
      | None -> failwith "cannot resolve a block name in t2")
  | _ -> resrho
and t2' cmd rho ns resrho =
  let blck = 
    SLM.find
    ns
    cmd
  in
  let nsls = (blck :Block.t).nested
  and resrho' = 
    List.fold_left
      (fun xrho rule ->
        t2'' cmd rho ns rule xrho
      )
      resrho
      (blck :Block.t).rules
  in
  List.fold_left
    (fun xrho xns ->
      t2' cmd rho xns xrho
    )
    resrho'
    nsls
let t2 cmd rho =
  t2' cmd rho ["#"] rho

let rec t3'' cmd rho ns sigma rule resrho = 
  match rule with
  | CILBLOCKINHERIT qn -> 
    (match eval_b_bar qn rho sigma with
      | Some (_, ns') ->
          let (_, ns'') = last_and_list qn in
          let rho' = t3' cmd rho ns' (sigma @ [ns'']) rho in
          rho_plus_rho ns qn resrho rho'
      | None -> failwith "cannot resolve a block name in t3")
  | (CILCALL (qn, pars)) ->
        let csi = cdmk qn rho sigma
        in
        rho_plus_csi resrho (List.hd sigma) csi
  | _ -> resrho
and t3' cmd rho ns sigma resrho =
  let blck = 
    SLM.find
    ns
    cmd
  in
  let nsls = (blck :Block.t).nested
  and resrho' = 
    List.fold_left
      (fun xrho rule ->
        t3'' cmd rho ns sigma rule xrho
      )
      resrho
      (blck :Block.t).rules
  in
  List.fold_left
    (fun xrho xns ->
      t3' cmd rho xns (xns :: (List.tl sigma)) xrho
    )
    resrho'
    nsls
let t3 cmd rho =
  t3' cmd rho ["#"] [["#"]] rho

let rec attrexp_map f = function
  | A_NAME n -> A_NAME (f n)
  | A_AND (a, a') -> A_AND (attrexp_map f a, attrexp_map f a')
  | A_OR (a, a') -> A_OR (attrexp_map f a, attrexp_map f a')
  | A_XOR (a, a') -> A_XOR (attrexp_map f a, attrexp_map f a')
  | A_NOT a -> A_NOT (attrexp_map f a)

let substitute rules fpars apars =
  let ftoapairs = (List.combine fpars apars) in
  let substitute_t qn = 
    match
      (List.find_opt 
        (fun (fp, ap) -> 
          match fp with
          | PARTYPE, pt -> qn = [pt]
          | _ -> false
        )
        ftoapairs)
    with
    | Some (fp, ap) -> ap
    | None -> qn
  in
  List.rev_map
    (function
    | CILCALL (mqn, apars) -> CILCALL 
        (
          mqn, 
          List.map substitute_t apars
        )
    | CILALLOW (src, dst, perms) -> CILALLOW (substitute_t src, substitute_t dst, perms)
    | CILATTRIBUTESET (attr, attrexp) -> CILATTRIBUTESET (attr, attrexp_map substitute_t attrexp)
    | r -> r)
  rules

let rec sem_Call_AR get_permissions cmd ns rules rho sigma csi allows = 
  List.fold_left
  (fun xallows rule ->
    sem_Call_A get_permissions cmd ns rule rho sigma csi xallows)
  allows
  rules

and sem_Call_A get_permissions cmd ns rule rho sigma csi allows =
  match rule with
  | CILCALL (qn, apars) ->
    let rho' = rho_intersec_csi rho [fresh] csi 
    in
    let (_, fpars, rules, csi', clousure) =
      option_value_error
        (eval_m_bar qn rho' ([fresh] :: sigma))
        "cannot resolve a macro name in sem_Call_A"
    in let eapars =
      option_value_error
        (eval_pars_bar apars fpars rho' ([fresh] :: sigma))
        "cannot resolve a macro parameters in sem_Call_A"
    in
    let rules' = substitute rules fpars eapars
    in
    sem_Call_AR get_permissions cmd ns rules' rho (clousure @ sigma) csi' allows
  | _ -> 
    let rho' = rho_intersec_csi rho [fresh] csi
    in
    sem_A'' get_permissions cmd rho' ns ([fresh]::sigma) rule allows 

and sem_A' get_permissions cmd rho ns sigma allows = 
  let blck = 
    SLM.find
    ns
    cmd
  in
  if (blck :Block.t).abstract then allows else
    (
      let nsls = (blck :Block.t).nested
      and allows' = 
      List.fold_left
        (fun xallows rule ->
          sem_A'' get_permissions cmd rho ns sigma rule xallows
        )
        allows
        (blck :Block.t).rules
      in
      List.fold_left
        (fun xallows xns ->
          let (b, _) = last_and_list xns in
          let fns = List.hd sigma in 
          let newsigma = (fns @ [b]) :: (List.tl sigma)
          in
          sem_A' get_permissions cmd rho xns newsigma xallows
        )
        allows'
        nsls
    )

and sem_A_i get_permissions cmd rho ns sigma allows = 
  let blck = 
    SLM.find
    ns
    cmd
  in
    let nsls = (blck :Block.t).nested
    and allows' = 
    List.fold_left
      (fun xallows rule ->
        sem_A'' get_permissions cmd rho ns sigma rule xallows
      )
      allows
      (blck :Block.t).rules
    in
    List.fold_left
      (fun xallows xns ->
        let (b, _) = last_and_list xns in
        let fns = List.hd sigma in 
        let newsigma = (fns @ [b]) :: (List.tl sigma)
        in
        sem_A_i get_permissions cmd rho xns newsigma xallows
      )
      allows'
      nsls

and sem_A'' get_permissions cmd rho ns sigma rule allows = 
  match rule with
  | CILALLOW (src, dst, perms) ->
      let gsrc =
        option_value_error
          (eval_tora_bar_E src rho sigma)
          "undefined type"
      and gdst = 
        option_value_error
          (eval_tora_bar_E dst rho sigma)
          "undefined type"
      in (gsrc, get_permissions perms, gdst) :: allows
  | CILBLOCKINHERIT qn ->
      (match eval_b_bar qn rho sigma with
      | Some (_, ns') ->
          let (_, ns'') = last_and_list qn in
          sem_A_i get_permissions cmd rho ns' (sigma @ [ns'']) allows
      | None -> failwith "cannot resolve a block name in sem_A")
  | CILCALL (qn, apars) ->
      let (mgn, fpars, rs, csi, clousure) = 
        option_value_error
          (eval_m_bar qn rho sigma)
          "cannot resolve a macro name in sem_A"
      in 
      let csi' = cdmk qn rho sigma 
      and csi'' = cdmk qn rho (clousure @ sigma) 
      and ns = List.hd sigma
      in
      let rho'' = rho_minus_csi rho ns csi''
      in
      let eapars =
        option_value_error
          (eval_pars_bar apars fpars rho'' sigma)
          "cannot resolve a macro parameters in sem_A"
      in
      let rho' = 
      fake_fr_rho rho ns csi' in
      let rs' = substitute rs fpars eapars in
      sem_Call_AR get_permissions cmd ns rs' rho' (clousure @ sigma) csi allows
  | _ -> allows

and sem_A get_permissions cmd rho =
  sem_A' get_permissions cmd rho ["#"] [["#"]] []

let sem_N rho =
  nss rho

let rec sem_Call_taR cmd ns rules rho sigma csi allows = 
  List.fold_left
  (fun xallows rule ->
    sem_Call_ta cmd ns rule rho sigma csi xallows)
  allows
  rules

and sem_Call_ta cmd ns rule rho sigma csi allows =
  match rule with
  | CILCALL (qn, apars) ->
    let rho' = rho_intersec_csi rho [fresh] csi 
    in
    let (_, fpars, rules, csi', clousure) =
      option_value_error
        (eval_m_bar qn rho' ([fresh] :: sigma))
        "cannot resolve a macro name in sem_Call_ta"
    in let eapars =
      option_value_error
        (eval_pars_bar apars fpars rho' ([fresh] :: sigma))
        "cannot resolve a macro parameters in sem_Call_ta"
    in
    let rules' = substitute rules fpars eapars
    in
    sem_Call_taR cmd ns rules' rho (clousure @ sigma) csi' allows
  | _ -> 
    let rho' = rho_intersec_csi rho [fresh] csi
    in
    sem_ta'' cmd rho' ns ([fresh]::sigma) rule allows 

and sem_ta' cmd rho ns sigma allows = 
  let blck = 
    SLM.find
    ns
    cmd
  in
  if (blck :Block.t).abstract then allows else
  (
    let nsls = 
      (blck :Block.t).nested
    and allows' = 
    List.fold_left
      (fun xallows rule ->
        sem_ta'' cmd rho ns sigma rule xallows
      )
      allows
      (blck :Block.t).rules
    in
    List.fold_left
      (fun xallows xns ->
        let (b, _) = last_and_list xns in
        let fns = List.hd sigma in 
        let newsigma = (fns @ [b]) :: (List.tl sigma)
        in
        sem_ta' cmd rho xns newsigma xallows
      )
      allows'
      nsls
  )

and sem_ta_i cmd rho ns sigma allows = 
  let blck = 
    SLM.find
    ns
    cmd
  in
  let nsls = 
    (blck :Block.t).nested
  and allows' = 
  List.fold_left
    (fun xallows rule ->
      sem_ta'' cmd rho ns sigma rule xallows
    )
    allows
    (blck :Block.t).rules
  in
  List.fold_left
    (fun xallows xns ->
      let (b, _) = last_and_list xns in
      let fns = List.hd sigma in 
      let newsigma = (fns @ [b]) :: (List.tl sigma)
      in
      sem_ta_i cmd rho xns newsigma xallows
    )
    allows'
    nsls

and sem_ta'' cmd rho ns sigma rule attrset = 
  match rule with
  | CILATTRIBUTESET (attr, expr) ->
      let gattr =
        option_value_error
          (eval_a_bar attr rho sigma)
          "undefined typeattribute"
      and gexpr = 
          eval_attrexp_E expr rho sigma
      in (gattr, gexpr) :: attrset
  | CILBLOCKINHERIT qn ->
      (match eval_b_bar qn rho sigma with
      | Some (_, ns') ->
          let (_, ns'') = last_and_list qn in
          sem_ta_i cmd rho ns' (sigma @ [ns'']) attrset
      | None -> failwith "cannot resolve a block name in sem_ta")
  | CILCALL (qn, apars) ->
      let (mgn, fpars, rs, csi, clousure) = 
        option_value_error
          (eval_m_bar qn rho sigma)
          "cannot resolve a macro name in sem_ta"
      in 
      let csi' = cdmk qn rho sigma 
      and csi'' = cdmk qn rho (clousure @ sigma) 
      and ns = List.hd sigma
      in
      let rho'' = rho_minus_csi rho ns csi''
      in
      let eapars =
        option_value_error
          (eval_pars_bar apars fpars rho'' sigma)
          "cannot resolve a macro parameters in sem_ta"
      in
      let rho' = 
      fake_fr_rho rho ns csi' in
      let rs' = substitute rs fpars eapars in
      sem_Call_taR cmd ns rs' rho' (clousure @ sigma) csi attrset
  | _ -> attrset
  
and sem_ta cmd rho =
    sem_ta' cmd rho ["#"] [["#"]] []

let add_ifl name iflreq iflm =
  SM.update
    name
    (function
      | Some xiflreq -> Some (IFL.meet xiflreq iflreq)
      | None -> Some iflreq)
    iflm

let rec sem_Call_iflR cmd ns rules rho sigma csi allows = 
  List.fold_left
  (fun xallows rule ->
    sem_Call_ifl cmd ns rule rho sigma csi xallows)
  allows
  rules

and sem_Call_ifl cmd ns rule rho sigma csi allows =
  match rule with
  | CILCALL (qn, apars) ->
    let rho' = rho_intersec_csi rho [fresh] csi 
    in
    let (_, fpars, rules, csi', clousure) =
      option_value_error
        (eval_m_bar qn rho' ([fresh] :: sigma))
        "cannot resolve a macro name in sem_Call_ifl"
    in let eapars =
      option_value_error
        (eval_pars_bar apars fpars rho' ([fresh] :: sigma))
        "cannot resolve a macro parameters in sem_Call_ifl"
    in
    let rules' = substitute rules fpars eapars
    in
    sem_Call_iflR cmd ns rules' rho (clousure @ sigma) csi' allows
  | _ -> 
    let rho' = rho_intersec_csi rho [fresh] csi
    in
    sem_ifl'' cmd rho' ns ([fresh]::sigma) rule allows 

and sem_ifl' cmd rho ns sigma allows = 
  let blck = 
    SLM.find
    ns
    cmd
  in
  if (blck :Block.t).abstract then allows else
  (
    let nsls = 
      (blck :Block.t).nested
    and allows' = 
    List.fold_left
      (fun xallows rule ->
        sem_ifl'' cmd rho ns sigma rule xallows
      )
      allows
      (blck :Block.t).rules
    in
    List.fold_left
      (fun xallows xns ->
        let (b, _) = last_and_list xns in
        let fns = List.hd sigma in 
        let newsigma = (fns @ [b]) :: (List.tl sigma)
        in
        sem_ifl' cmd rho xns newsigma xallows
      )
      allows'
      nsls
  )

and sem_ifl_i cmd rho ns sigma allows = 
  let blck = 
    SLM.find
    ns
    cmd
  in
  let nsls = 
    (blck :Block.t).nested
  and allows' = 
  List.fold_left
    (fun xallows rule ->
      sem_ifl'' cmd rho ns sigma rule xallows
    )
    allows
    (blck :Block.t).rules
  in
  List.fold_left
    (fun xallows xns ->
      let (b, _) = last_and_list xns in
      let fns = List.hd sigma in 
      let newsigma = (fns @ [b]) :: (List.tl sigma)
      in
      sem_ifl_i cmd rho xns newsigma xallows
    )
    allows'
    nsls

and sem_ifl'' cmd rho ns sigma rule iflreqs = 
  match rule with
  | IFL (name, req) ->
      let greq = 
        eval_ifl_E req rho sigma
      in 
      add_ifl name greq iflreqs
  | CILBLOCKINHERIT qn ->
      (match eval_b_bar qn rho sigma with
      | Some (_, ns') ->
          let (_, ns'') = last_and_list qn in
          sem_ifl_i cmd rho ns' (sigma @ [ns'']) iflreqs
      | None -> failwith "cannot resolve a block name in sem_ifl")
  | CILCALL (qn, apars) ->
      let (mgn, fpars, rs, csi, clousure) = 
        option_value_error
          (eval_m_bar qn rho sigma)
          "cannot resolve a macro name in sem_ifl"
      in 
      let csi' = cdmk qn rho sigma 
      and csi'' = cdmk qn rho (clousure @ sigma) 
      and ns = List.hd sigma
      in
      let rho'' = rho_minus_csi rho ns csi''
      in
      let eapars =
        option_value_error
          (eval_pars_bar apars fpars rho'' sigma)
          "cannot resolve a macro parameters in sem_ifl"
      in
      let rho' = 
      fake_fr_rho rho ns csi' in
      let rs' = substitute rs fpars eapars in
      sem_Call_iflR cmd ns rs' rho' (clousure @ sigma) csi iflreqs
  | _ -> iflreqs
  
and sem_ifl cmd rho =
    sem_ifl' cmd rho ["#"] [["#"]] SM.empty
    
let get_semantics rules = 
  (* print_endline "######### RULES #########\n";
  CILsyntax.print rules;
  print_endline "###############################\n"; *)

  let cmds = from_config rules  in
  (* print_endline "######### COMMANDS #########\n";
  CILsyntaxE.print cmds;
  print_endline "###############################\n"; *)

  let get_permissions = permissions cmds in
  
  let irho = initialrho rules in
  (* print_endline "######### INITIAL RHO #########\n";
  CILenvE.print irho;
  print_endline "###############################\n"; *)

  let cmds' = t1 cmds irho in     
  (* print_endline "######### RULES AFTER T1 #########\n";
  CILsyntaxE.print cmds';
  print_endline "###############################\n"; *)

  let rho' = t2 cmds' irho in 
  (* print_endline "\n######### RHO AFTER T2 #########\n";
  CILenvE.print rho';
  print_endline "###############################\n"; *)

  let rho'' = t3 cmds' rho' in 
  (* print_endline "######### RHO AFTER T3 #########\n";
  CILenvE.print rho'';
  print_endline "###############################\n"; *)

  let nodes = sem_N rho''
  and allows = sem_A get_permissions cmds' rho''  
  and attributes = sem_ta cmds' rho'' 
  and iflreqs = sem_ifl cmds' rho'' in
  let ops = 
    List.fold_left
      (fun xops (_, o, _) -> 
        SS.union xops o
        )
      SS.empty
      allows
  in 
  (* print_endline "######### ALLOWS AFTER E_A #########\n";
  (* List.iter
    (fun (src,dst) -> 
      print_string ("permission from " ^ (qn_tostring src) ^ " to " ^ (qn_tostring dst) ^ "\n"))
    allows; *)
  print_int (List.length nodes);
  print_int (List.length attributes);
  print_endline "###############################\n"; *)

  print_endline "SEMANTIC COMPLETED ";
  {
    nodes = nodes;
    allows = allows;
    ta = attributes;
    ifl = iflreqs;
    bigo = ops;
  }


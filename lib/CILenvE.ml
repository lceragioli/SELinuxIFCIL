open IFCILsyntax
open Utils

let enterblock ns dn =
  List.rev (dn :: (List.rev ns))

type sigma = ns list

type csi = {
  types : SS.t;
  attributes : SS.t;
}

let ruleset_union rs1 rs2 =
  List.sort_uniq 
    (fun stm1 stm2 ->
      compare stm1 stm2) 
    (List.rev_append rs1 rs2)

let csi_union csi1 csi2 = {
  types = SS.union csi1.types csi2.types;
  attributes = SS.union csi1.attributes csi2.attributes;
}

type tval = qn
type aval = qn
type mval = qn * ((parametertype * string) list) * (statement list) * csi * sigma
type bval = qn * ns

let rec csi_from_ruleset' csi = function
  | [] -> csi
  | rule :: rules ->
      (match rule with
        | CILTYPE t ->
            csi_from_ruleset' {csi with types = SS.add t csi.types } rules
        | CILATTRIBUTE a -> 
            csi_from_ruleset' {csi with attributes = SS.add a csi.attributes } rules
        | _ -> 
          csi_from_ruleset' csi rules)

let csi_from_ruleset = 
  csi_from_ruleset' { types = SS.empty; attributes = SS.empty }

module Frame = struct
  type t = 
  { 
    trho: tval SM.t;
    arho: aval SM.t;
    brho: bval SM.t;
    mrho: mval SM.t;
  }

  let emptyframe = {trho = SM.empty; arho = SM.empty; brho = SM.empty; mrho = SM.empty }
  let update_t dn v fr = {fr with trho = SM.add dn v fr.trho} 
  let update_a dn v fr = {fr with arho = SM.add dn v fr.arho} 
  let update_m dn v fr = {fr with mrho = SM.add dn v fr.mrho} 
  let update_b dn v fr = {fr with brho = SM.add dn v fr.brho} 

  let update_gn ns { trho; arho; brho; mrho } =
    let update_ns qn =
      (
        let (dn, _) = last_and_list qn
        in
        ns @ [dn]
      )
    in
    {
      trho = SM.map (fun gn -> update_ns gn) trho; 
      arho = SM.map (fun gn -> update_ns gn) arho;
      brho = SM.map (fun (gn, rs) -> (update_ns gn, rs)) brho; 
      mrho = SM.empty
    }

  let update_gn_m ns { trho; arho; brho; mrho } =
    let update_ns qn =
      (
        let (dn, _) = last_and_list qn
        in
        ns @ [dn]
      )
    in
    {
      trho = SM.empty; 
      arho = SM.empty;
      brho = SM.empty; 
      mrho = SM.map (fun (gn, pars, rs, csi, sigma) -> (update_ns gn, pars, rs, csi, ns :: sigma)) mrho;
    }

  let remove { trho; arho; brho; mrho } csi =
    {
      trho = SM.filter (fun dn _ -> not (SS.exists (fun dn' -> dn = dn') csi.types)) trho; 
      arho = SM.filter (fun dn _ -> not (SS.exists (fun dn' -> dn = dn') csi.attributes)) arho;
      brho;
      mrho;
    }

  let intersec { trho; arho; brho; mrho } csi =
    {
      trho = SM.filter (fun dn _ -> SS.exists (fun dn' -> dn = dn') csi.types) trho; 
      arho = SM.filter (fun dn _ -> SS.exists (fun dn' -> dn = dn') csi.attributes) arho;
      brho;
      mrho;
    }
  

  let merge fr1 fr2 =
    { 
      trho = SM.union (fun _ fn _ -> Some fn) fr1.trho fr2.trho;
      arho = SM.union (fun _ fn _ -> Some fn) fr1.arho fr2.arho;
      brho = SM.union (fun _  (fn1, rs1) (fn2, rs2) -> Some (fn1, rs1)) fr1.brho fr2.brho;
      mrho = SM.union 
        (fun _  (fn1, pars1, rs1, csi1, sigma1) (fn2, pars2, rs2, csi2, sigma2) -> 
            Some (fn1, pars1, ruleset_union rs1 rs2, csi_union csi1 csi2, sigma1 @ sigma2)) 
        fr1.mrho fr2.mrho;
    }

  let find_opt_t dn fr = SM.find_opt dn fr.trho
  let find_opt_a dn fr = SM.find_opt dn fr.arho
  let find_opt_m dn fr = SM.find_opt dn fr.mrho
  let find_opt_b dn fr = SM.find_opt dn fr.brho

  let print_frame {trho; arho; brho; mrho } =
    (print_string "\ntypes\n";
    SM.iter
      (fun dn qn -> 
        print_string (dn ^ ":" ^ (String.concat "." qn) ^ "\n"))
      trho;
    print_string "\nattributes\n";
    SM.iter
      (fun dn qn -> 
        print_string (dn ^ ":" ^ (String.concat "." qn) ^ "\n"))
      arho;
    print_string "\nblocks\n";
    SM.iter
        (fun dn (qn,rs) -> 
          print_string (dn ^ ":" ^ (String.concat "." qn) ^ "\n");
          print_endline (String.concat "." rs)
        )
        brho;
      print_string "\nmacros\n";
      SM.iter
        (fun dn (qn,_,rs,csi,sigma) -> 
          print_string "macroname: ";
          print_string (dn ^ ":" ^ (String.concat "." qn) ^ "\n");
          print_string "rules:\n";
          print rs;
          print_string "csi.types = ";
          print_endline (String.concat "; " (SS.elements csi.types));
          print_string "csi.attributes = ";
          print_endline (String.concat "; " (SS.elements csi.attributes));
        )
        mrho;)

  let from_csi csi ns rhons_fr =
    {
      trho = 
        (SS.fold
          (fun dn fr -> 
            match find_opt_t dn rhons_fr with
            | Some v -> SM.add dn (List.rev (dn :: List.rev(ns))) fr
            | None -> failwith "boh - types"
          )
          csi.types
          SM.empty
        );

      arho =
        (SS.fold
          (fun dn fr -> 
            match find_opt_a dn rhons_fr with
            | Some v -> SM.add dn v fr
            | None -> failwith "boh - attributes"
          )
          csi.attributes
          SM.empty);
      mrho = SM.empty;
      brho = SM.empty;
    }

end

type rho = Frame.t SLM.t 

let union = SLM.union (fun _ fr1 fr2 -> Some (Frame.merge fr1 fr2))

let print r = 
  SLM.iter
    (fun ns f ->
      print_string "beginning of the frame for -- ";
      print_string 
        (String.concat "." ns);
      print_string " -- ";
      Frame.print_frame f;
      print_newline ();   
      print_string "end of the frame for -- ";
      print_string 
       (String.concat "." ns);
      print_string " -- ";
      print_newline ();   
    )
  r

let applyrho selec qn ns rho =
  let (dn, ns') = last_and_list qn in
  let ans = ns @ ns'
  in
  match SLM.find_opt ans rho with
  | Some fr -> selec dn fr
  | None -> None

let rec eval selec qn ns rho =
  if ns = ["#"] || ns = [] then None else
  match applyrho selec qn ns rho with
  | Some v -> Some v
  | None -> 
      let (_, ns') = last_and_list ns in
      eval selec qn ns' rho

let eval_tora_static =
  eval (fun fr dn ->
    match Frame.find_opt_t fr dn with
    | Some qn -> Some qn
    | None -> Frame.find_opt_a fr dn)

let rec eval_bar selec qn rho sigma =
  flush_all ();
  match sigma with
  | [] -> 
      let qn' = if List.hd qn = "#" then List.tl qn else qn in
      applyrho selec qn' ["#"] rho
  | ns :: sigma' -> (
    match eval selec qn ns rho with
    | Some v -> Some v
    | None -> eval_bar selec qn rho sigma')

let eval_t_bar = eval_bar Frame.find_opt_t
let eval_a_bar = eval_bar Frame.find_opt_a
let eval_m_bar = eval_bar Frame.find_opt_m
let eval_b_bar = eval_bar Frame.find_opt_b

let eval_tora_bar = eval_bar 
  (fun fr dn ->
    match Frame.find_opt_t fr dn with
    | Some qn -> Some qn
    | None -> Frame.find_opt_a fr dn)

let eval_tora_bar_E = eval_bar 
  (fun fr dn ->
    match Frame.find_opt_t fr dn with
    | Some qn -> Some (A_Type qn)
    | None -> 
        Option.map
          (fun x -> A_Attr x) 
          (Frame.find_opt_a fr dn)
  )

let opt_apply f oa ob =
  match (oa, ob) with
  | Some a, Some b -> Some (f a b)
  | _ -> None

let rec eval_pars_bar' rho sigma = function
  | [] -> Some []
  | (qn, (PARTYPE, _)) :: pairs ->
      opt_apply List.cons (eval_tora_bar qn rho sigma) (eval_pars_bar' rho sigma pairs)
  | (qn, _) :: pairs -> Option.map (fun x -> qn :: x) (eval_pars_bar' rho sigma pairs)

let eval_pars_bar apars fpars rho sigma =
  eval_pars_bar' rho sigma (List.combine apars fpars)


let rec commands rules =
  List.filter_map(
    fun r -> match r with
    | CILBLOCKINHERIT _
    | CILTYPEALIASACTUAL _
    | CILATTRIBUTESET _
    | CILCALL _
    | CILALLOW _ -> Some r
    | CILBLOCK (dn, rules') ->
        let cmds = commands rules'
        in
        if cmds = [] then None else Some (CILBLOCK (dn, cmds))
    | _ -> None
  )
  rules

let eval_if_not_in ns rho rule csi =
  let eval qn = 
    if SS.exists
      (fun dn -> [dn] = qn)
      (SS.union csi.types csi.attributes)
    then qn
    else
    Option.value
      (eval_tora_static qn rho ns)
      ~default: qn
  in
  match rule with
  | CILALLOW (src, dst, perms) ->
      let gsrc = eval src
      and gdst = eval dst
  in CILALLOW (gsrc, gdst, perms)
    (* anche call e altra roba andrebbero qui, XXX TODO *)
  | _ -> rule

let rec initialrhoR' rules ns =
  List.fold_left
  (fun xrho xrho' -> union xrho xrho')
  SLM.empty
  (List.rev_map
    (fun rule -> initialrhoR rule ns)
    rules)

and initialrhoR rule ns =
  match rule with
    | CILTYPE st -> 
          (SLM.add
            ns 
            (Frame.update_t st (List.rev (st :: List.rev ns)) Frame.emptyframe)
            SLM.empty)
    | CILATTRIBUTE st -> 
      (SLM.add
        ns 
        (Frame.update_a st (List.rev (st :: List.rev ns)) Frame.emptyframe)
        SLM.empty)
    | CILMACRO (st, pars, rules) -> 
      let mval = 
        (
          List.rev (st :: List.rev ns), 
          pars, 
          commands rules, 
          csi_from_ruleset rules,
          [ns]
        ) 
      in
      (SLM.add
        ns 
        (Frame.update_m st mval Frame.emptyframe)
        SLM.empty)    
    | CILBLOCK (bdn, rules) ->
          let rho = initialrhoR' rules (List.rev (bdn::(List.rev ns)))
          and bval = 
            (
              List.rev (bdn :: List.rev ns), 
              List.rev (bdn :: List.rev ns)
            ) 
          in
            SLM.update
              ns 
                (function 
                  | Some fr -> Some (Frame.update_b bdn bval fr)
                  | None -> Some (Frame.update_b bdn bval Frame.emptyframe))
              rho
    | _ -> SLM.empty

let initialrho rules =
  initialrhoR' rules ["#"]

let rec cdmk' qnmls rho sigma k =
  match qnmls with
  | [] -> k (csi_from_ruleset [])
  | qnm :: qnmls' ->
      match eval_m_bar qnm rho sigma with
      | Some (mfn, pars, rules, csi, clousure) ->
          let qnmls'' = 
            List.filter_map
              (function
                | CILCALL (qnm', _) -> Some qnm'
                | _ -> None
              )
              rules
          in
          cdmk' qnmls' rho sigma (fun xcsi ->
            cdmk' qnmls'' rho (clousure @ sigma) (fun xcsi' ->
              k(csi_union xcsi (csi_union csi xcsi'))
            ))
      | None -> 
        failwith ("fail resolving macroname in cdm " ^ (qn_tostring qnm))

let cdmk qnm rho sigma =
  cdmk' [qnm] rho sigma (fun x -> x)

let rho_plus_csi rho ns csi =
  (* callrho rho ns csi returns rho[ns + {(dn, ns.dn) | dn in csi}] *)
  let update_frame fr =
    (
    let frwitht = 
      (SS.fold
      (fun tdn xfr' -> Frame.update_t tdn (enterblock ns tdn) xfr')
      csi.types
      fr) in
    let frwithta =
      (SS.fold
      (fun adn xfr' -> Frame.update_a adn (enterblock ns adn) xfr')
      csi.attributes
      frwitht) 
    in
      frwithta
    )
  in
  SLM.update ns
    (function
      | Some xfr -> Some (update_frame xfr)
      | None -> Some (update_frame Frame.emptyframe)
    )
    rho

let rho_minus_csi rho ns csi =
  (* returns ρ [ns ↦ ρ(ns) \ ξ] *)
  SLM.update ns
    (function 
      | Some xfr ->
          Some (Frame.remove xfr csi)
      | None -> None
    )
  rho

let rho_intersec_csi rho ns csi =
  (* returns ρ [ns ↦ ρ(ns) ∩ ξ] *)
  SLM.update ns
    (function 
      | Some xfr ->
          Some (Frame.intersec xfr csi)
      | None -> None
    )
  rho

let rho_plus_rho ns gn rho rho' =
  (* returns rho[ns + rho'(ns')]  
      -- also updating the globally qualified names in rho'(ns')
      -- and update the contexts of macros
    More formally, it returns rho[ns + upd(rho'(ns'), ns)]
    Old: More formally, it returns rho[ns + {(dn, val_{ns.dn}) | (dn, val) in rho'(ns')}]
  *)
  SLM.fold
    (fun xns' fr' xrho -> 
      match listminus xns' gn with
        | None -> xrho
        | Some qn -> 
            let fr_updated =
              Frame.update_gn (ns @ qn) fr'
            in
            SLM.update
            (ns @ qn)
            (function
              | Some xfr -> Some (Frame.merge xfr fr_updated)
              | None -> Some fr_updated)
            xrho
      )
    rho'
    rho

let rho_plus_rho_m ns gn rho rho' =
  SLM.fold
    (fun xns' fr' xrho -> 
      match listminus xns' gn with
        | None -> xrho
        | Some qn -> 
            let fr_updated =
              Frame.update_gn_m (ns @ qn) fr'
            in
            SLM.update
            (ns @ qn)
            (function
              | Some xfr -> Some (Frame.merge xfr fr_updated)
              | None -> Some fr_updated)
            xrho
      )
    rho'
    rho
let fresh = "---fresh---"

let fake_fr_rho rho ns csi =
  (* old -- fake_fr_rho rho mgn ns csi : returns rho[mgn |-> {(dn, rho(ns)(dn)) | dn in csi}] *)
  (* fake_fr_rho ρ ns ξ returns ρ [fresh ↦ {(dn, ρ(ns)(dn)) | dn ∈ ξ}] *)
  let rhons_fr = 
    match SLM.find_opt ns rho with
    | Some nsfr -> nsfr
    | None -> Frame.emptyframe
  in
  let fr = Frame.from_csi csi ns rhons_fr 
  in
  SLM.update
    [fresh]
    (function
    | Some xfr -> Some (Frame.merge xfr fr)
    | None -> Some fr)
  rho

let nss rho =
  let nodeset =
    SLM.fold(
      fun _ (fr :Frame.t) xnss ->
        (
          SM.fold
          (
            fun _ t xnss' ->
              SLS.add t xnss'
          )
          fr.trho
          xnss
        )
    )
    rho
    SLS.empty
  in SLS.elements nodeset

let ass rho =
  let nodeset =
    SLM.fold(
      fun _ (fr :Frame.t) xnss ->
        (
          SM.fold
          (
            fun _ a xnss' ->
              SLS.add a xnss'
          )
          fr.arho
          xnss
        )
    )
    rho
    SLS.empty
  in SLS.elements nodeset 

(* let rec eval_attrexp attrexp rho sigma =
  match (attrexp :attributeexp) with
  | A_NAME qn -> 
      let gn = 
        option_value_error
        (eval_tora_bar qn rho sigma)
        "undefined name in attributexp"
      in (A_NAME gn: attributeexp)
  | A_OR (attrexp', attrexp'') -> A_OR (eval_attrexp attrexp' rho sigma, eval_attrexp attrexp'' rho sigma)
  | A_AND (attrexp', attrexp'') -> A_AND (eval_attrexp attrexp' rho sigma, eval_attrexp attrexp'' rho sigma)
  | A_XOR (attrexp', attrexp'') -> A_XOR (eval_attrexp attrexp' rho sigma, eval_attrexp attrexp'' rho sigma)
  | A_NOT attrexp -> A_NOT (eval_attrexp attrexp rho sigma) *)

let rec eval_attrexp_E attrexp rho sigma =
  match (attrexp :attributeexp) with
  | A_NAME qn -> 
      let aortgn = 
        option_value_error
        (eval_tora_bar_E qn rho sigma)
        "undefined name in attributexp"
      in E_NAME aortgn
  | A_OR (attrexp', attrexp'') -> E_OR (eval_attrexp_E attrexp' rho sigma, eval_attrexp_E attrexp'' rho sigma)
  | A_AND (attrexp', attrexp'') -> E_AND (eval_attrexp_E attrexp' rho sigma, eval_attrexp_E attrexp'' rho sigma)
  | A_XOR (attrexp', attrexp'') -> E_XOR (eval_attrexp_E attrexp' rho sigma, eval_attrexp_E attrexp'' rho sigma)
  | A_NOT attrexp -> E_NOT (eval_attrexp_E attrexp rho sigma)

(* let eval_iflp iflp rho sigma = 
  List.map
  (fun (p1, ma, p2) ->
    let p1g = if p1 = ["any-node"] then p1 else
      option_value_error
      (eval_tora_bar p1 rho sigma)
      ("undefined name in iflp : " ^ (String.concat "." p1))
    and p2g = if p2 = ["any-node"] then p2 else
      option_value_error
      (eval_tora_bar p2 rho sigma)
      ("undefined name in iflp : " ^ (String.concat "." p2))
      in
    (p1g, ma, p2g))
  iflp

let eval_ifl (iflr:iflreq) rho sigma =
  match iflr with
  | MUST p -> MUST (eval_iflp p rho sigma)
  | MUSTNOT p -> MUSTNOT (eval_iflp p rho sigma)
  | EVERYMUST (p, p') -> EVERYMUST (eval_iflp p rho sigma, eval_iflp p' rho sigma) *)


let eval_iflp_E (iflp:iflpath) rho sigma = 
  List.map
  (fun (p1, ma, p2) ->
    let p1g = if p1 = ["any-node"] then Any else
      option_value_error
      (eval_tora_bar_E p1 rho sigma)
      ("undefined name in iflp : " ^ (String.concat "." p1))
    and p2g = if p2 = ["any-node"] then Any else
      option_value_error
      (eval_tora_bar_E p2 rho sigma)
      ("undefined name in iflp : " ^ (String.concat "." p2))
      in
    (p1g, ma, p2g))
  iflp

let eval_ifl_E (iflr:iflreq) rho sigma =
  match iflr with
  | MUST p -> E_MUST (eval_iflp_E p rho sigma)
  | MUSTNOT p -> E_MUSTNOT (eval_iflp_E p rho sigma)
  | EVERYMUST (p, p') -> E_EVERYMUST (eval_iflp_E p rho sigma, eval_iflp_E p' rho sigma)
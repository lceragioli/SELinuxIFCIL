type parametertype = 
    PARTYPE 
  | PARCLASS 
  | PARTYPEALIAS
  | PARCLASSPERMISSION
  (* classpermission(named or anonymous), 
   block, name (a string), *)
  | PARCLASSMAP
  | PARIGNORE

type path = string list

type attributeexp = 
    A_NAME of string list
  | A_OR of attributeexp * attributeexp
  | A_AND of attributeexp * attributeexp
  | A_XOR of attributeexp * attributeexp
  | A_NOT of attributeexp

  type classpermissionsetcon =
  | Permissions of string list
  | Expression of attributeexp

  type classpermission =
  | Name of string list
  | Anonym of path * classpermissionsetcon

  type statement = 
    CILTYPE of string
  | CILTYPEALIAS of string
  | CILTYPEALIASACTUAL of string * path
  | CILATTRIBUTE of string
  | CILATTRIBUTESET of path * attributeexp
  | CILBLOCK of string * (statement list)
  | CILBLOCKINHERIT of path
  | CILBLOCKABSTRACT
  | CILCALL of path * (path list)
  | CILMACRO of string * ((parametertype * string) list) * (statement list)
  | CILALLOW of path * path * classpermission
  | CILIN of path * (statement list)
  | CILCOMMON of string * string list
  | CILCLASSCOMMON of path * path
  | CILCLASS of string * string list
  | CILCLASSPERMISSION of string
  | CILCLASSPERMISSIONSET of path * path * classpermissionsetcon
  | CILCLASSMAP of string * string list
  | CILCLASSMAPPING of path * path * classpermission

  (* let rec fold' f v ns_rs_pairs = 
    match ns_rs_pairs with
    | [] -> v
    | (ns, []) :: ns_rs_pairs' -> fold' f v ns_rs_pairs'
    | (ns, rule :: ruleset) :: ns_rs_pairs' -> 
        let update_frame fr = 
          (match rule with
            | CILTYPE st -> (Frame.update_t st (st :: ns) fr)
            | CILATTRIBUTE st -> (Frame.update_a st (st :: ns) fr)
            | CILMACRO (st, pars, rules) -> 
                (Frame.update_m 
                  st 
                  (
                    st :: ns, 
                    pars, 
                    rules, 
                    csi_from_ruleset rules
                  ) 
                  fr)
            | CILBLOCK (st, rules) -> 
              (Frame.update_b 
                  st 
                  (
                    st :: ns, 
                    rules 
                  ) 
                  fr)
            | _ -> fr)
          and ns_rs_pairs'' = 
              (match rule with
                | CILBLOCK (b, rules) -> (b::ns, rules)::((ns, ruleset) :: ns_rs_pairs')
                | _ -> ((ns, ruleset) :: ns_rs_pairs'))
        in
          let rho' =
            SLM.update
              ns 
              (function 
                | Some fr -> Some (update_frame fr)
                | None -> Some (update_frame Frame.emptyframe))
              rho
        in
        initialrho' rho' ns_rs_pairs'' *)

(* let fold ruleset f u =
    fold' f u [(["#"], ruleset)] *)

exception InUndefinedBlock




let removeIN rules = 
  let additions = 
    List.fold_left
      (fun pairs -> function
        | CILIN (ns, rules) -> 
            let gns = 
              if List.hd ns = "#" then List.tl ns
              else ns
            in
              (gns, rules) :: pairs
        | _ -> pairs)
      []
      rules
  in let rec add_additions (ans, arules) = function
    | [] -> 
        let rec genblocks rs = function
          | [] -> raise InUndefinedBlock
          | a :: [] ->  CILBLOCK (a, rs)
          | a :: ans' -> 
              CILBLOCK (a, [genblocks rs ans'])       
        in
        [genblocks arules ans]
        (* if you don't find it, you add the block - it's arbitrary like all the rest of this language *)
    | CILBLOCK (dn, rs) :: rules ->
            if dn = "unconfined" then print_string "checcazzo!\n";
            (match ans with
              | [] -> failwith "error, (in ..) statement not refferring to any actual block"
              | adn :: ans' ->
                  if dn = adn then
                    if ans' = [] then
                      CILBLOCK (dn, List.rev_append rs arules) :: rules
                    else
                      CILBLOCK (dn, add_additions (ans', arules) rs) :: rules
                  else
                    CILBLOCK (dn, rs) :: (add_additions (ans, arules) rules))
    | _ :: rules -> add_additions (ans, arules) rules 
  in 
    List.fold_left
      (fun rules' addition ->
        try
          add_additions addition  
          rules'
        with
        | InUndefinedBlock -> 
            match addition with (a,r) -> failwith ("error, (in ..) statement not refferring to any actual block : " ^ (String.concat "." a))
      )
      rules
      additions

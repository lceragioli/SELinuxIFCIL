open IFCILsyntax
open Utils
open IFCILsyntaxE

module SP = OrderPair (String) (String)
module SPM = Map.Make (SP)

let rec get' add cmd ns gots =
  let blck = 
    SLM.find
    ns
    cmd
  in
  let nsls = (blck :Block.t).nested
  and gots' = 
    List.fold_left
      (fun xgots rule ->
        add xgots rule
      )
      gots
      (blck :Block.t).rules
  in
  List.fold_left
    (fun xgots xns ->
      get' add cmd xns xgots
    )
    gots'
    nsls

let get zero add cmds =
  get' add cmds ["#"] zero

let get_commons =
  get 
    SM.empty 
    (fun xcommons rule ->
      match rule with
      | CILCOMMON (name, permissions) -> 
          SM.add
            name 
            (SS.of_list permissions)
            xcommons
      | _ -> xcommons
    )

let get_classes =
  get 
    SM.empty 
    (fun xlcasses rule ->
      match rule with
      | CILCLASS (name, permissions) -> 
          SM.add
            name 
            (SS.of_list permissions)
            xlcasses
      | _ -> xlcasses
    )

let get_classes_with_commons cls coms =
  get 
    cls
    (fun xclasses rule ->
      match rule with
      | CILCLASSCOMMON (cls, com) -> 
          let clsname = (List.nth cls 0)
          and comname = (List.nth com 0) in
          let perms = (SM.find comname coms) in
          SM.update
            clsname
            (
              function
              | None -> Some perms
              | Some perms' -> Some 
                  (SS.union perms perms')
            )
            xclasses
      | _ -> xclasses
    )

let rec perm_filter' allperms ex = 
  match (ex:attributeexp) with
  | A_NAME name -> 
    let nname = List.nth name 0 in
    if nname = "all" then allperms
    else SS.singleton nname
  | A_AND (ex1, ex2) -> 
      let perms1 = perm_filter' allperms ex1
      and perms2 = perm_filter' allperms ex2
      in SS.inter perms1 perms2 
  | A_OR (ex1, ex2) -> 
    let perms1 = perm_filter' allperms ex1
    and perms2 = perm_filter' allperms ex2
    in SS.union perms1 perms2 
  | A_XOR (ex1, ex2) -> 
    let perms1 = perm_filter' allperms ex1
    and perms2 = perm_filter' allperms ex2
    in 
      SS.diff
        (SS.union perms1 perms2)
        (SS.inter perms1 perms2)
  | A_NOT ex -> 
    let perms = perm_filter' allperms ex
    in 
      SS.diff
        allperms
        perms

let perm_filter allperms = function
    Permissions perms -> SS.of_list perms
  | Expression attrexp -> perm_filter' allperms attrexp

let get_class_permissions clss =
  get 
    SM.empty 
    (fun xclassperms rule ->
      match rule with
      | CILCLASSPERMISSIONSET (clps, cls, clscon) -> 
          let clpsname = (List.nth clps 0)
          and clsname = (List.nth cls 0) in
          let allperms = (SM.find clsname clss) in
          let perms = perm_filter allperms clscon in
          SM.update
            clpsname
            (
              function
              | None -> Some perms
              | Some perms' -> Some (SS.union perms perms')
            )
            xclassperms
      | _ -> xclassperms
    )

let eval_classpermission cpex clss cpms clsmis =
  match cpex with
    | Name qn -> 
        let cpname = List.nth qn 0 in
        SM.find cpname cpms 
    | Anonym (qn, clsperm) ->
        let firstname = List.nth qn 0 in
        try (
          let allperms = (SM.find firstname clss) in
          perm_filter allperms clsperm
          )
        with Not_found ->
          (
            match clsperm with
              | Permissions nls ->
                  let secondname = List.nth nls 0 in
                  SPM.find (firstname, secondname) clsmis
              | Expression ex ->
                  match ex with
                    | A_NAME nls ->
                      let secondname = List.nth nls 0 in
                      SPM.find (firstname, secondname) clsmis
                    | _ -> failwith "impossible to resolve a permission"
          )

let get_classmapping clss cpms =
  get 
  SPM.empty 
  (fun xclassmapping rule ->
    match rule with
    | CILCLASSMAPPING (clm, clmi, cpmex) -> 
        let clmname = (List.nth clm 0)
        and clminame = (List.nth clmi 0) in
        let perms = eval_classpermission cpmex clss cpms xclassmapping in
        SPM.update
          (clmname, clminame)
          (
            function
            | None -> Some perms
            | Some perms' -> Some (SS.union perms perms')
          )
          xclassmapping
    | _ -> xclassmapping
  )

let permissions cmds = 
  let commons = get_commons cmds
  and iclasses = get_classes cmds in
  let classes = get_classes_with_commons iclasses commons cmds in
  let class_permissions = get_class_permissions classes cmds in
  let classmapping = get_classmapping classes class_permissions cmds in

  function clsperm -> 
    eval_classpermission clsperm classes class_permissions classmapping

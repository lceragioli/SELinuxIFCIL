type parametertype = 
    PARTYPE 
  | PARCLASS 
  | PARTYPEALIAS
  | PARCLASSPERMISSION
  (* classpermission(named or anonymous), 
   block, name (a string), *)
  | PARCLASSMAP
  | PARIGNORE

type dn = string
type qn = dn list
type ns = dn list

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
  | Anonym of qn * classpermissionsetcon

type arrow = 
    LONGARROW
  | SHORTARROW
type marrow = string list * arrow
type path = string list
type iflpiece = 
    IFLNODE of path
  | IFLARROW of marrow
type iflstep = path * marrow * path
type iflpath = iflstep list
type iflpieces = iflpiece list
type iflreq =
    MUSTNOT of iflpath
  | MUST of iflpath
  | EVERYMUST of iflpath * iflpath

type statement = 
    CILTYPE of string
  | CILTYPEALIAS of string
  | CILTYPEALIASACTUAL of string * qn
  | CILATTRIBUTE of string
  | CILATTRIBUTESET of qn * attributeexp
  | CILBLOCK of string * (statement list)
  | CILBLOCKINHERIT of qn 
  | CILBLOCKABSTRACT
  | CILCALL of qn * (qn list) 
  | CILMACRO of string * ((parametertype * string) list) * (statement list)
  | CILALLOW of qn * qn * classpermission
  | CILIN of qn * (statement list)
  | IFL of string * iflreq
  | CILCOMMON of string * string list
  | CILCLASSCOMMON of qn * qn
  | CILCLASS of string * string list
  | CILCLASSPERMISSION of string
  | CILCLASSPERMISSIONSET of qn * qn * classpermissionsetcon
  | CILCLASSMAP of string * string list
  | CILCLASSMAPPING of qn * qn * classpermission

type torat_node =
  Any
| A_Type of qn
| A_Attr of qn

type attributeexpE = 
  E_NAME of torat_node 
| E_OR of attributeexpE * attributeexpE
| E_AND of attributeexpE * attributeexpE
| E_XOR of attributeexpE * attributeexpE
| E_NOT of attributeexpE

type iflstepE = torat_node * marrow * torat_node
type iflpathE = iflstepE list

type iflreqE =
  E_MUSTNOT of iflpathE
| E_MUST of iflpathE
| E_EVERYMUST of iflpathE * iflpathE


exception InUndefinedBlock

let removeIN' rules =
  List.filter
    (function 
      | CILIN (_, _) -> false
      | _ -> true
    )
    rules

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
            (match ans with
              | [] -> failwith "error, (in ..) statement not refferring to any actual block"
              | adn :: ans' ->
                  if dn = adn then
                    (if ans' = [] then
                      CILBLOCK (dn, List.rev_append rs arules) :: rules
                    else
                      CILBLOCK (dn, add_additions (ans', arules) rs) :: rules
                    )
                  else
                    CILBLOCK (dn, rs) :: (add_additions (ans, arules) rules))
    | rule :: rules -> rule :: (add_additions (ans, arules) rules)
  in 
    removeIN'  
    (
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
    )

let qn_tostring qn =
  String.concat "." qn

let classperm_tostring = function
  | Name qls -> String.concat "." qls
  | _ -> ""

let macroapar_tostring apar =
  String.concat
  " "
  (List.map
    (fun qn -> "(" ^ (qn_tostring qn) ^ ")")
    apar
  )
  
let macrofpar_tostring fpar =
  String.concat
  " "
  (List.map
    (function
      | (PARTYPE, dn) -> "(type " ^ dn ^ ")"
      | _ -> ""
    )
    fpar
  )

let rec print' rules k =
      match rules with
      | [] -> k ""
      | CILBLOCK (dn, rs) :: rules ->
          print' rs 
          (fun xrs -> print' rules (
            fun xrs' -> k (
              "(block " ^ dn ^ xrs ^ "\n)\n" ^ xrs')
          ))
      | CILMACRO (dn, par, rs) :: rules ->
          print' rs 
          (fun xrs -> print' rules (
            fun xrs' -> k (
              "(macro " ^ dn ^ " (" ^ (macrofpar_tostring par) ^ ")" ^ xrs ^ "\n)\n" ^ xrs')
          ))
      | CILTYPE t :: rules -> 
          print' rules (fun xrs -> k ("(type " ^ t ^ ")\n" ^ xrs))
      | CILBLOCKINHERIT qn :: rules -> 
          print' rules (fun xrs -> k ("(blockinherit " ^ (qn_tostring qn) ^ ")\n" ^ xrs))
      | CILATTRIBUTE t :: rules -> 
          print' rules (fun xrs -> k ("(typeattribute " ^ t ^ ")\n" ^ xrs))
      | CILALLOW (t, t', p) :: rules -> 
          print' rules (fun xrs -> k ("(allow " ^ (qn_tostring t) ^ (qn_tostring t') ^ (classperm_tostring p) ^ ")\n" ^ xrs))
      | CILCALL (m, par) :: rules -> 
          print' rules (fun xrs -> k ("(call " ^ (qn_tostring m) ^ "(" ^ (macroapar_tostring par) ^ "))\n" ^ xrs))
      | CILATTRIBUTESET (qn, aexp) :: rules -> 
          print' rules (fun xrs -> k ("(typeattributeset " ^ (qn_tostring qn) ^ "( something ))\n" ^ xrs))
      | _ :: rules -> 
              print' rules k
                 
let print rules = print' rules (fun str -> print_endline str)

let rec conf_to_string' rules k =
  match rules with
  | [] -> k ""
  | CILBLOCK (dn, rs) :: rules ->
      conf_to_string' rs 
      (fun xrs -> conf_to_string' rules (
        fun xrs' -> k (
          "(block " ^ dn ^ " " ^ xrs ^ "\n)\n" ^ xrs')
      ))
  | CILMACRO (dn, par, rs) :: rules ->
      conf_to_string' rs 
      (fun xrs -> conf_to_string' rules (
        fun xrs' -> k (
          "(macro " ^ dn ^ " (" ^ (macrofpar_tostring par) ^ ")" ^ xrs ^ "\n)\n" ^ xrs')
      ))
  | CILTYPE t :: rules -> 
      conf_to_string' rules (fun xrs -> k ("(type " ^ t ^ ")\n" ^ xrs))
  | CILBLOCKINHERIT qn :: rules -> 
      conf_to_string' rules (fun xrs -> k ("(blockinherit " ^ (qn_tostring qn) ^ ")\n" ^ xrs))
  | CILATTRIBUTE t :: rules -> 
      conf_to_string' rules (fun xrs -> k ("(typeattribute " ^ t ^ ")\n" ^ xrs))
  | CILALLOW (t, t', p) :: rules -> 
      conf_to_string' rules (fun xrs -> k ("(allow " ^ (qn_tostring t) ^ " " ^ (qn_tostring t') ^ " (file(read)))\n" ^ xrs))
  | CILCALL (m, par) :: rules -> 
      conf_to_string' rules (fun xrs -> k ("(call " ^ (qn_tostring m) ^ "(" ^ (macroapar_tostring par) ^ "))\n" ^ xrs))
  | _ :: rules -> 
      conf_to_string' rules k

let conf_to_string rules = conf_to_string' rules (fun str -> str)

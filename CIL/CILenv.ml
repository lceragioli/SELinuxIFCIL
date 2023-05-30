open CILsyntax

module SS = Set.Make (String)

type dn = string
type qn = dn list
type csi = {
  types : dn list;
  attributes : dn list;
}

type tval = qn
type aval = qn
type mval = qn * ((parametertype * string) list) * (statement list) * csi
type bval = qn * (statement list)

let csi_from_ruleset' csi = function
  | [] -> csi
  | rule :: rules ->
      match rule with
        | CILTYPE t ->  {csi with types = t :: csi.types }
        | CILATTRIBUTE a ->  {csi with attributes = a :: csi.attributes }
        | _ -> csi

let csi_from_ruleset ruleset = 
  csi_from_ruleset' { types = []; attributes = [] } ruleset

module OrderList (Ord : Stdlib__set.OrderedType) = struct
  include List
  type t = Ord.t list
  let compare = List.compare Ord.compare
end;;

module OrderPair (Ord1 : Stdlib__set.OrderedType) (Ord2 : Stdlib__set.OrderedType) = struct
  type t = Ord1.t * Ord2.t
  let compare (a, b) (c, d) = 
    if Ord1.compare a c == 0 then Ord2.compare b d else Ord1.compare a c
end;;

module StringList = OrderList (String)
(* module SLS = Set.Make (StringList) *)

module StSL = OrderPair (String) (StringList)
(* module StSLS = Set.Make (StSL) *)

module SM = Map.Make (String)
module SLM = Map.Make (StringList)

type ns = dn list
type sigma = ns list

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

  let print_frame {trho; arho; brho; mrho } =
    print_string "\ntypes\n";
    SM.iter
      (fun dn qn -> 
        print_string (dn ^ ":" ^ (String.concat "." (List.rev qn)) ^ "\n"))
      trho;
    print_string "\nattributes\n";
    SM.iter
      (fun dn qn -> 
        print_string (dn ^ ":" ^ (String.concat "." (List.rev qn)) ^ "\n"))
      arho;
    print_string "\nblocks\n";
    SM.iter
        (fun dn (qn,_) -> 
          print_string (dn ^ ":" ^ (String.concat "." (List.rev qn)) ^ "\n"))
        brho;
      print_string "\nmacros\n";
      SM.iter
        (fun dn (qn,_,_,_) -> 
          print_string (dn ^ ":" ^ (String.concat "." (List.rev qn)) ^ "\n"))
        mrho;
end

type rho = Frame.t SLM.t 

let rec initialrho' rho ns_rs_pairs = 
  match ns_rs_pairs with
  | [] -> rho
  | (ns, []) :: ns_rs_pairs' -> initialrho' rho ns_rs_pairs'
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
      initialrho' rho' ns_rs_pairs''

let initialrho ruleset =
    initialrho' SLM.empty [(["#"], ruleset)]


let print r = 
  SLM.iter
    (fun ns f ->
      print_string 
        (String.concat "." (List.rev ns));
      print_string " -- ";
      Frame.print_frame f;
      print_newline ();   
    )
  r
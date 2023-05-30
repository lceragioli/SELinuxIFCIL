open CILsyntax

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

module SS = Set.Make (String)

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
module SLS = Set.Make (StringList)

module StSL = OrderPair (String) (StringList)
module StSLS = Set.Make (StSL)

type rhoT = 
{ 
  trho: int;
  arho: int;
  brho: int;
  mrho: int;
}


module SM = Map.Make (String)
module SLM = Map.Make (StringList)
let testrho = SLM.add ["aa"] (SM.add "s" ({trho = 1;
  arho = 2;
  brho = 3;
  mrho = 4;
}) SM.empty) SLM.empty 
module StSLSM = Map.Make (StSLS)



(* type valT = TYPE | ATTRIBUTE | BLOCK | MACRO
module OrdervalT = struct
  type t = valT
  let compare a b = compare a b
end;;

module ValtStSLSM = Map.Make (OrdervalT)

type rhoT = ValtStSLSM.t *)

type ns = dn list
type sigma = ns list

module type Rho = sig
  type t
  type tf
  val eval : t -> sigma -> qn -> 'a
  val from_CIL : statement list -> t
  val get_f : t -> ns -> tf
  val update : t -> ns -> tf -> t
end

module Rho = struct
  type valT = TYPE | ATTRIBUTE | BLOCK | MACRO
  type rhoT = 
    { 
      trho: tval StSLSM.t;
      arho: aval StSLSM.t;
      brho: bval StSLSM.t;
      mrho: mval StSLSM.t;
    }

  let update rho valT f =
    match rho with
    | { trho; arho; brho; mrho } ->
        match valT with
        | TYPE -> { rho with trho = f trho }
        | ATTRIBUTE -> { trho = t; arho = f a; brho = b; mrho = m }
        | BLOCK -> { trho = t; arho = a; brho = f b; mrho = m }
        | MACRO -> { trho = t; arho = a; brho = b; mrho = f m }
  let initialrho rule rho ns =
    match rule with
    | CILTYPE of string -> rho.tval 
    | CILTYPEALIAS of string
    | CILTYPEALIASACTUAL of string * path
    | CILATTRIBUTE of string
    | CILATTRIBUTESET of path * attributeexp
    | CILBLOCK of string * (statement list)
    | CILBLOCKINHERIT of path * refinements
    | CILBLOCKABSTRACT
    | CILCALL of path * (path list) * refinements
    | CILMACRO of string * ((parametertype * string) list) * (statement list)
    | CILALLOW of path * path * classpermission
    | IFL of string * iflreq
    | CILIN of path * (statement list)
    | CILCOMMON of string * string list
    | CILCLASSCOMMON of path * path
    | CILCLASS of string * string list
    | CILCLASSPERMISSION of string
    | CILCLASSPERMISSIONSET of path * path * classpermissionsetcon
    | CILCLASSMAP of string * string list
    | CILCLASSMAPPING of path * path * classpermission
open IFCILsyntax
open Utils

type semantics =
  {
    nodes: qn list;
    allows: (torat_node * SS.t * torat_node) list;
    ta: (qn * attributeexpE) list;
    ifl: iflreqE SM.t;
    bigo: SS.t;
  }

val get_semantics : statement list -> semantics

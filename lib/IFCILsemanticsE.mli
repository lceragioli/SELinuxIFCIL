open IFCILsyntax
open Utils

type semantics =
{
  types: qn list;
  attributes: qn list;
  allows: (torat_node * SS.t * torat_node) list;
  ta: attributeexpE SLM.t;
  ifl: iflreqE SM.t;
  bigo: SS.t
}

val get_semantics : statement list -> semantics

open IFCILsyntax

type csi
type sigma = ns list

type tval = qn
type aval = qn
type mval = qn * ((parametertype * string) list) * (statement list) * csi * sigma
type bval = qn * ns

type rho

val enterblock : qn -> dn -> qn

val eval_tora_bar : qn -> rho -> sigma -> qn option
val eval_tora_bar_E : qn -> rho -> sigma -> torat_node option
val eval_t_bar : qn -> rho -> sigma -> tval option
val eval_a_bar : qn -> rho -> sigma -> aval option
val eval_m_bar : qn -> rho -> sigma -> mval option
val eval_b_bar : qn -> rho -> sigma -> bval option
val eval_pars_bar : qn list -> ((parametertype * dn) list) -> rho -> sigma -> qn list option
(* val eval_attrexp : attributeexp -> rho -> sigma -> attributeexp *)
val eval_attrexp_E : attributeexp -> rho -> sigma -> attributeexpE
(* val eval_ifl : iflreq -> rho -> sigma -> iflreq *)
val eval_ifl_E : iflreq -> rho -> sigma -> iflreqE

val union : rho -> rho -> rho
val print : rho -> unit
val initialrho : statement list -> rho
val rho_plus_rho_m : ns -> ns -> rho -> rho -> rho
val rho_plus_rho : ns -> ns -> rho -> rho -> rho
(* rhoplus ns ns' rho rho' returns rho[ns + rho'(ns')] *)

val cdmk : qn -> rho -> sigma -> csi

val rho_plus_csi : rho -> qn -> csi -> rho
val rho_minus_csi : rho -> ns -> csi -> rho
val rho_intersec_csi : rho -> ns -> csi -> rho
val fake_fr_rho : rho -> ns -> csi -> rho
(* fake_fr_rho rho mgn ns csi returns rho[mgn |-> {(dn, rho(ns)(dn)) | dn in csi}] *)

val fresh : string

val nss : rho -> string list list

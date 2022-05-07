val meet : CILsyntax.iflreq -> CILsyntax.iflreq -> CILsyntax.iflreq

val kind_meet :
  (string list * (string list * CILsyntax.arrow) * string list) list ->
  (string list * (string list * CILsyntax.arrow) * string list) list ->
  (string list * (string list * CILsyntax.arrow) * string list) list

val minor : CILsyntax.iflreq -> CILsyntax.iflreq -> bool

val meet : CILgrammar.iflreq -> CILgrammar.iflreq -> CILgrammar.iflreq

val kind_meet :
  (string list * (string list * CILgrammar.arrow) * string list) list ->
  (string list * (string list * CILgrammar.arrow) * string list) list ->
  (string list * (string list * CILgrammar.arrow) * string list) list

val minor : CILgrammar.iflreq -> CILgrammar.iflreq -> bool

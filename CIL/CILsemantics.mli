open CILsyntax

type dn
type qn
type csi

type tval = qn
type aval = qn
type mval = qn * ((parametertype * string) list) * (statement list) * csi
type mval = qn * (statement list)

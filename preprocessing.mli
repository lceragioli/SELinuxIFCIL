open IFCILconfiguration
open CILgrammar

exception OurError of string

exception UnsupportedConstruct of string

exception UndefinedReference of string

exception UndefinedMeet of string

exception NotUniqueMeet of string

exception UncorrectRefinement of string

type semi_flat_statement =
  | FLAT of flat_statement
  | SEMIFLATIN of path * statement list

val flatten_conf : CILgrammar.statement list ->
  string list -> (string list * IFCILconfiguration.flat_statement) list

open IFCILconfiguration
open CILsyntax
open Utils

exception OurError of string

exception UnsupportedConstruct of string

exception UndefinedReference of string

exception UndefinedMeet of string

exception NotUniqueMeet of string

exception UncorrectRefinement of string

val flatten_conf : CILsyntax.statement list ->
  string list -> (string list * flat_statement) list

val ifcil_configuration : statement list -> flat_statement list SLM.t

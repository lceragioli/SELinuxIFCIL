open Utils
open IFCILconfiguration

exception UndefinedMeet of string

exception NotUniqueMeet of string

exception UncorrectRefinement of string

val normalize : flat_statement list SLM.t ->
  flat_statement list SLM.t

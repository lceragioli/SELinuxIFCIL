exception UndefinedMeet of string

exception NotUniqueMeet of string

exception UncorrectRefinement of string

val normalize : (string list * IFCILconfiguration.flat_statement) list ->
  (string list * IFCILconfiguration.flat_statement) list

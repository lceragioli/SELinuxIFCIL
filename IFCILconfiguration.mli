open CILsyntax

exception OurError of string

exception UnsupportedConstruct of string

exception UndefinedReference of string

(* Types:
  path - is a list of scope names (macro and block names), e.g., A.B.m 
  flat_statement - any of the commands of the language, where commands with nested commands are flattened,
      e.g., (block A (type a)) is indeed (block A), occurring in the global path, and (type a), occurring inside A 
*)

type flat_statement =
  | FLATTYPE of string
  | FLATTYPEALIAS of string
  | FLATTYPEALIASACTUAL of string * string list
  | FLATATTRIBUTE of string
  | FLATATTRIBUTESET of path * attributeexp
  | FLATBLOCKABSTRACT
  | FLATBLOCK of string
  | FLATBLOCKINHERIT of path * refinements
  | FLATCALL of path * path list * refinements
  | FLATMACRO of string * (parametertype * string) list
  | FLATALLOW of path * path * classpermission
  | FLATIFL of string * iflreq
  | FLATCOMMON of string * string list
  | FLATCLASSCOMMON of string list * string list
  | FLATCLASS of string * string list
  | FLATCLASSPERMISSION of string
  | FLATCLASSPERMISSIONSET of string list * string list * classpermissionsetcon
  | FLATCLASSMAP of string * string list
  | FLATCLASSMAPPING of string list * string list * classpermission

(* ------------------- name resolution ------------------- *)

val eval_macr :
  string list ->
  CILgrammar.token ->
  string list ->
  (string list * flat_statement) list ->
  string list
(* eval_macr pos construct qname fstmntls
   parameters:
     pos - the path in which the name to be resolved occurs
     construct - the kind of construct we are resolving, may be a type, a typeattribute etc
     qname - the name to be resolved, it is a full name, may be A.c.d, #.B.d, d, etc
     fstmntls - the consfiguration we are considering

   return the real name of the entity of kind construct named with qname appearing in pos WHICH IS A MACRO, i.e., a name starting with #
*)

val eval_bl :
  string list ->
  CILgrammar.token ->
  string list ->
  (string list * flat_statement) list ->
  string list
(* let rec eval_bl pos construct qname fstmntls
   parameters:
     pos - the path in which the name to be resolved occurs
     construct - the kind of construct we are resolving, may be a type, a typeattribute etc
     qname - the name to be resolved, it is a full name, may be A.c.d, #.B.d, d, etc
     fstmntls - the consfiguration we are considering

   return the real name of the entity of kind construct named with qname appearing in pos WHICH IS A BLOCK, i.e., a name starting with #
*)

val eval_bl_type_attr :
  string list ->
  CILgrammar.token ->
  string list ->
  (string list * flat_statement) list ->
  string list
(* let eval_bl_type_attr pos construct name fstmntls
   parameters:
     pos - the path in which the name to be resolved occurs
     construct - in lots of contexts, types and typeattributes can appear one in place oif another;
       if construct is TYPE than we are looking for a type and if not defined for an attribute, if construct is TYPEATTRIBUTE than we are looking for an attribute
     name - the name to be resolved, it is a (relative or absolute) full name, may be A.c.d, #.B.d, d, etc
     fstmntls - the consfiguration we are considering

   return the real name of the entity of kind construct named with qname appearing in pos WHICH IS A BLOCK, i.e., a name starting with #
*)

(* ------------------- definition checking ------------------- *)

val is_block : string list -> (string list * flat_statement) list -> bool
(* is_block qname fstmntls
   parameters:
     qname - absolutely qualified name of the BLOCK, i.e., #.A.B.C
     fstmntls - consfiguration in hand

   returns true if such a block is defined in the configuration
*)

val is_type : string list -> (string list * flat_statement) list -> bool
(* is_type t pos fstmntls
   parameters:
     qname - absolutely qualified name of the TYPE, i.e., #.A.B.a
     fstmntls - consfiguration in hand

   returns true if such a type is defined in the configuration
*)

val is_attribute : string list -> (string list * flat_statement) list -> bool
(* is_type t pos fstmntls
   parameters:
     qname - absolutely qualified name of the TYPATTRIBUTE, i.e., #.A.B.a
     fstmntls - consfiguration in hand

   returns true if such a typeattribute is defined in the configuration
*)

(* ------------------- normalization ------------------- *)

val remove_recursion : ('a * flat_statement) list -> ('a * flat_statement) list
(* remove_recursion fstmntls
    returns an equivalent versione of the configuration in input where undefined recursion in the definitions
    of attributes is removed
*)

(* ------------------- metadata access ------------------- *)

val all_defined_operations :
  ('a * flat_statement) list -> Stdlib__set.Make(Stdlib__string).t
(* returns the set of the names of all the defined operations in the input configuration *)

val filter_operations :
  Stdlib__set.Make(Stdlib__string).t ->
  CILsyntax.attributeexp ->
  Stdlib__set.Make(Stdlib__string).t
(* given a set of operations and an expression
   returns the subset of operations defined bu the expression
*)

val operations_of_clspermsetcon :
  CILsyntax.classpermissionsetcon ->
  string list ->
  (string list * flat_statement) list ->
  string list
(* take the operation for cls and filter them using clssetcon *)

val ops_in_class :
  string list -> (string list * flat_statement) list -> string list option
(*
   ops_in_class qname fstmntls
   parameters:
     qname - a absolutly qualified name of a CLASS
     fstmntls - a configuration

   returns the list of operations defined on the class
*)

val operations :
  CILsyntax.classpermission ->
  (string list * flat_statement) list ->
  (string list * string) list
(*
   Given a classpermission and a configuration,
   returns the list of pairs (cls, operation) of classes and operations defined on them
*)

(* -------------- Printing functions -------------- *)

val print_IFL :
  string list * (string list * CILsyntax.arrow) * string list -> string

val print_IFL_requirement : CILsyntax.iflreq -> string

val print_IFL_refinements :
  (string * string list * CILsyntax.iflreq) list -> string

val print_attrset : CILsyntax.attributeexp -> string

val print_path : string list -> string

val print_classpermissionsetcon : CILsyntax.classpermissionsetcon -> string

val print_classpermission : CILsyntax.classpermission -> string

val print_fparams : (CILsyntax.parametertype * string) list -> string

val print_flat_CIL : string list * flat_statement -> string

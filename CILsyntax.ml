type parametertype = 
    PARTYPE 
  | PARCLASS 
  | PARTYPEALIAS
  | PARCLASSPERMISSION
  (* classpermission(named or anonymous), 
   block, name (a string), *)
  | PARCLASSMAP
  | PARIGNORE
type arrow = 
    LONGARROW
  | SHORTARROW
type marrow = string list * arrow
type path = string list
type iflpiece = 
    IFLNODE of path
  | IFLARROW of marrow
type iflstep = path * marrow * path
type iflpath =
    iflstep list
type iflpieces =
    iflpiece list
type iflreq =
    MUSTNOT of iflpath
  | MUST of iflpath
  | EVERYMUST of iflpath * iflpath
type refinements = ((string * (string list) * iflreq) list)
type attributeexp = 
    A_NAME of string list
  | A_OR of attributeexp * attributeexp
  | A_AND of attributeexp * attributeexp
  | A_XOR of attributeexp * attributeexp
  | A_NOT of attributeexp
type classpermissionsetcon =
  | Permissions of string list
  | Expression of attributeexp
type classpermission =
  | Name of string list
  | Anonym of path * classpermissionsetcon
type statement = 
    CILTYPE of string
  | CILTYPEALIAS of string
  | CILTYPEALIASACTUAL of string * path
  | CILATTRIBUTE of string
  | CILATTRIBUTESET of path * attributeexp
  | CILBLOCK of string * (statement list)
  | CILBLOCKINHERIT of path * refinements
  | CILBLOCKABSTRACT
  | CILCALL of path * (path list) * refinements
  | CILMACRO of string * ((parametertype * string) list) * (statement list)
  | CILALLOW of path * path * classpermission
  | IFL of string * iflreq
  | CILIN of path * (statement list)
  | CILCOMMON of string * string list
  | CILCLASSCOMMON of path * path
  | CILCLASS of string * string list
  | CILCLASSPERMISSION of string
  | CILCLASSPERMISSIONSET of path * path * classpermissionsetcon
  | CILCLASSMAP of string * string list
  | CILCLASSMAPPING of path * path * classpermission
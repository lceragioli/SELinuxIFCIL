type token =
  | NAME of (string)
  | TYPE
  | BLOCK
  | IN
  | COMMON
  | CLASSCOMMON
  | CLASS
  | CLASSPERMISSION
  | CLASSPERMISSIONSET
  | CLASSMAP
  | CLASSMAPPING
  | BLOCKABSTRACT
  | TYPEALIAS
  | TYPEALIASACTUAL
  | BLOCKINHERIT
  | MACRO
  | CALL
  | ALLOW
  | SELF
  | ATTRIBUTE
  | ATTRIBUTESET
  | ALL
  | ANOT
  | AND
  | OR
  | XOR
  | LPAREN
  | RPAREN
  | STRING
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> CILsyntax.statement list

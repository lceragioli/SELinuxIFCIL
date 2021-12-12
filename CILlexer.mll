(* File lexer.mll *)
{
open CILgrammar        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    | ";IFL;"               { IFL }
    | [' ' '\t' '\n'] | 
        ((";\n" ) | 
            (';' [^ '\n'] '\n' ) | 
            (';' [^ '\n'] [^ '\n'] '\n' ) |
            (';' [^ '\n'] [^ '\n'] '\n' ) |
            (';' [^ 'I' '\n'] [^ '\n'] [^ '\n'] ([^ '\n'])* '\n') |
            (';' [^ '\n'] [^ 'F' '\n'] [^ '\n'] ([^ '\n'])* '\n') |
            (';' [^ '\n'] [^ '\n'] [^ 'L' '\n'] ([^ '\n'])* '\n'))  { token lexbuf }     (* skip blanks and comments *)
    | "type"                { TYPE }
    | "block"               { BLOCK }
    | "in"                  { IN }
    | "common"              { COMMON }
    | "classcommon"         { CLASSCOMMON }
    | "class"               { CLASS }
    | "classpermission"     { CLASSPERMISSION }
    | "classpermissionset"  { CLASSPERMISSIONSET }
    | "classmap"            { CLASSMAP }
    | "classmapping"        { CLASSMAPPING }
    | "blockabstract"       { BLOCKABSTRACT }
    | "in"                  { IN }
    | "typealias"           { TYPEALIAS }
    | "typealiasactual"     { TYPEALIASACTUAL }
    | "blockinherit"        { BLOCKINHERIT }
    | "macro"               { MACRO }
    | "call"                { CALL }
    | "allow"   
        | "auditallow"      { ALLOW }
    | "self"                { SELF }
    | "typeattribute"       { ATTRIBUTE }
    | "typeattributeset"    { ATTRIBUTESET } 
    | "all"                 { ALL }
    | "not"                 { ANOT }
    | "and"                 { AND }
    | "or"                  { OR }
    | "xor"                 { XOR }
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | "-"                   { ARROWBODY } 
    | ">"                   { ARROWHEAD }
    | "["                   { LSQUARE }
    | "]"                   { RSQUARE }
    | "~"                   { NOT }
    | ":"                   { COLON }
    | ","                   { COMMA }
    | ("*" | (['0'-'9'] | ['a'-'z'] | ['A' - 'Z'] | '.' | '_' | '-' | ',' | '/' | '\\' )) + | ('\"' [^ '\"']* '\"') as lxm { NAME(lxm) }
    | eof                   { EOF }

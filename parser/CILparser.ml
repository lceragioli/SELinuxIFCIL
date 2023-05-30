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

open Parsing;;
let _ = parse_error;;
# 2 "parser/CILparser.mly"
open CILsyntax

let read_path str =
    if str = "*" then ["any-node"] else
    if (String.get str 0) = '.' 
        then String.split_on_char '.' ("#" ^ str)
    else
        String.split_on_char '.' str
# 45 "parser/CILparser.ml"
let yytransl_const = [|
  258 (* TYPE *);
  259 (* BLOCK *);
  260 (* IN *);
  261 (* COMMON *);
  262 (* CLASSCOMMON *);
  263 (* CLASS *);
  264 (* CLASSPERMISSION *);
  265 (* CLASSPERMISSIONSET *);
  266 (* CLASSMAP *);
  267 (* CLASSMAPPING *);
  268 (* BLOCKABSTRACT *);
  269 (* TYPEALIAS *);
  270 (* TYPEALIASACTUAL *);
  271 (* BLOCKINHERIT *);
  272 (* MACRO *);
  273 (* CALL *);
  274 (* ALLOW *);
  275 (* SELF *);
  276 (* ATTRIBUTE *);
  277 (* ATTRIBUTESET *);
  278 (* ALL *);
  279 (* ANOT *);
  280 (* AND *);
  281 (* OR *);
  282 (* XOR *);
  283 (* LPAREN *);
  284 (* RPAREN *);
  285 (* STRING *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\005\000\
\005\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\003\000\002\000\002\000\003\000\005\000\
\004\000\003\000\005\000\004\000\002\000\008\000\006\000\005\000\
\009\000\007\000\004\000\002\000\003\000\003\000\002\000\002\000\
\003\000\006\000\005\000\005\000\002\000\009\000\007\000\004\000\
\009\000\007\000\004\000\001\000\002\000\003\000\003\000\004\000\
\004\000\005\000\005\000\005\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\004\000\004\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\110\000\
\000\000\000\000\071\000\056\000\055\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\000\000\000\000\095\000\085\000\
\083\000\075\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\084\000\086\000\087\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\005\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\000\000\000\000\024\000\006\000\000\000\
\023\000\000\000\000\000\000\000\020\000\000\000\000\000\001\000\
\003\000\072\000\000\000\000\000\022\000\025\000\000\000\010\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
\000\000\045\000\000\000\021\000\004\000\074\000\009\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\035\000\000\000\032\000\000\000\
\000\000\000\000\000\000\000\000\000\000\097\000\008\000\011\000\
\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\028\000\000\000\000\000\039\000\
\000\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\000\000\000\000\000\000\000\000\040\000\
\041\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\031\000\042\000\043\000\044\000\014\000\000\000\109\000\104\000\
\105\000\107\000\108\000\106\000\000\000\000\000\017\000\033\000\
\030\000"

let yydgoto = "\002\000\
\024\000\025\000\026\000\120\000\161\000\116\000\130\000\100\000"

let yysindex = "\029\000\
\169\000\000\000\061\000\244\000\244\000\006\255\244\000\244\000\
\244\000\244\000\244\000\244\000\244\000\244\000\244\000\244\000\
\244\000\244\000\244\000\244\000\244\000\244\000\169\000\000\000\
\043\000\169\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\140\255\061\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\169\000\169\000\017\255\244\000\
\018\255\000\000\020\255\021\255\244\000\000\000\000\000\244\000\
\000\000\022\255\023\255\222\000\000\000\005\255\040\255\000\000\
\000\000\000\000\168\255\061\000\000\000\000\000\196\255\000\000\
\224\255\244\000\244\000\088\000\000\000\014\255\244\000\115\000\
\142\000\000\000\035\255\000\000\000\000\000\000\000\000\244\000\
\041\255\000\000\042\255\007\255\044\255\244\000\000\000\075\255\
\169\000\047\255\050\255\244\000\000\000\244\000\000\000\033\000\
\056\255\005\255\005\255\005\255\005\255\000\000\000\000\000\000\
\196\000\059\255\000\000\008\255\064\255\066\255\088\255\089\255\
\091\255\092\255\000\000\169\000\000\000\037\255\039\255\000\000\
\067\255\000\000\068\255\005\255\005\255\005\255\033\000\056\255\
\071\255\000\000\196\000\072\255\073\255\074\255\076\255\077\255\
\079\255\081\255\000\000\196\000\082\255\196\000\083\255\000\000\
\000\000\085\255\087\255\098\255\099\255\100\255\000\000\090\255\
\090\255\090\255\090\255\090\255\090\255\101\255\000\000\103\255\
\000\000\000\000\000\000\000\000\000\000\105\255\000\000\000\000\
\000\000\000\000\000\000\000\000\106\255\107\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\108\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\001\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\109\255\
\111\255\187\255\191\255\192\255\193\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\230\255\093\000\006\000\182\255\171\255\184\255\009\000"

let yytablesize = 544
let yytable = "\097\000\
\029\000\036\000\002\000\037\000\073\000\114\000\078\000\114\000\
\114\000\076\000\077\000\054\000\079\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\090\000\
\091\000\092\000\093\000\094\000\121\000\001\000\123\000\115\000\
\125\000\145\000\171\000\136\000\131\000\114\000\146\000\114\000\
\128\000\129\000\096\000\103\000\105\000\142\000\106\000\107\000\
\110\000\111\000\101\000\102\000\163\000\164\000\165\000\166\000\
\137\000\138\000\139\000\140\000\141\000\099\000\172\000\180\000\
\173\000\182\000\174\000\117\000\143\000\144\000\169\000\147\000\
\181\000\183\000\156\000\149\000\150\000\157\000\186\000\187\000\
\188\000\151\000\152\000\162\000\153\000\104\000\170\000\154\000\
\175\000\176\000\108\000\177\000\178\000\109\000\184\000\185\000\
\190\000\113\000\189\000\191\000\192\000\193\000\155\000\194\000\
\195\000\198\000\196\000\200\000\197\000\199\000\201\000\124\000\
\202\000\127\000\203\000\095\000\128\000\133\000\135\000\207\000\
\208\000\209\000\210\000\211\000\212\000\204\000\205\000\206\000\
\213\000\179\000\214\000\148\000\215\000\216\000\217\000\096\000\
\103\000\158\000\098\000\159\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\098\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\118\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\099\000\073\000\
\074\000\075\000\101\000\102\000\100\000\000\000\000\000\119\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\000\000\073\000\074\000\075\000\000\000\000\000\
\000\000\000\000\000\000\122\000\000\000\000\000\000\000\000\000\
\000\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\000\000\029\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\000\036\000\002\000\037\000\
\073\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\072\000\000\000\073\000\074\000\075\000\000\000\
\000\000\000\000\000\000\000\000\160\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\000\000\073\000\074\000\075\000\000\000\000\000\
\000\000\000\000\126\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\000\000\073\000\074\000\
\075\000\000\000\000\000\000\000\000\000\132\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\000\000\073\000\074\000\075\000\000\000\000\000\000\000\000\000\
\134\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\000\000\021\000\022\000\000\000\000\000\
\000\000\000\000\000\000\023\000\167\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\000\000\073\000\
\074\000\168\000\138\000\139\000\140\000\141\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\112\000\073\000\074\000\075\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\000\000\073\000\
\074\000\075\000\094\000\094\000\094\000\094\000\094\000\094\000\
\094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
\094\000\094\000\094\000\094\000\000\000\094\000\094\000\094\000"

let yycheck = "\026\000\
\000\000\000\000\000\000\000\000\000\000\001\001\001\001\001\001\
\001\001\004\000\005\000\003\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\021\000\022\000\103\000\001\000\105\000\027\001\
\107\000\027\001\027\001\001\001\111\000\001\001\124\000\001\001\
\027\001\028\001\000\000\027\001\027\001\120\000\027\001\027\001\
\027\001\027\001\077\000\078\000\138\000\139\000\140\000\141\000\
\022\001\023\001\024\001\025\001\026\001\053\000\148\000\027\001\
\001\001\027\001\001\001\028\001\028\001\028\001\145\000\028\001\
\158\000\159\000\028\001\001\001\002\001\028\001\164\000\165\000\
\166\000\007\001\008\001\028\001\010\001\080\000\028\001\013\001\
\001\001\001\001\085\000\001\001\001\001\088\000\028\001\028\001\
\171\000\092\000\028\001\028\001\028\001\028\001\129\000\028\001\
\028\001\180\000\028\001\182\000\028\001\028\001\028\001\106\000\
\028\001\108\000\028\001\023\000\027\001\112\000\113\000\192\000\
\193\000\194\000\195\000\196\000\197\000\028\001\028\001\028\001\
\028\001\156\000\028\001\126\000\028\001\028\001\028\001\028\001\
\028\001\132\000\028\001\134\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\028\001\020\001\
\021\001\022\001\028\001\028\001\028\001\255\255\255\255\028\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\020\001\021\001\022\001\255\255\255\255\
\255\255\255\255\255\255\028\001\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\020\001\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\028\001\028\001\028\001\028\001\
\028\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\020\001\021\001\022\001\255\255\
\255\255\255\255\255\255\255\255\028\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\020\001\021\001\022\001\255\255\255\255\
\255\255\255\255\027\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\020\001\021\001\
\022\001\255\255\255\255\255\255\255\255\027\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\027\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\020\001\021\001\255\255\255\255\
\255\255\255\255\255\255\027\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\020\001\
\021\001\022\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\020\001\021\001\022\001"

let yynames_const = "\
  TYPE\000\
  BLOCK\000\
  IN\000\
  COMMON\000\
  CLASSCOMMON\000\
  CLASS\000\
  CLASSPERMISSION\000\
  CLASSPERMISSIONSET\000\
  CLASSMAP\000\
  CLASSMAPPING\000\
  BLOCKABSTRACT\000\
  TYPEALIAS\000\
  TYPEALIASACTUAL\000\
  BLOCKINHERIT\000\
  MACRO\000\
  CALL\000\
  ALLOW\000\
  SELF\000\
  ATTRIBUTE\000\
  ATTRIBUTESET\000\
  ALL\000\
  ANOT\000\
  AND\000\
  OR\000\
  XOR\000\
  LPAREN\000\
  RPAREN\000\
  STRING\000\
  EOF\000\
  "

let yynames_block = "\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmnts) in
    Obj.repr(
# 21 "parser/CILparser.mly"
                ( _1 )
# 394 "parser/CILparser.ml"
               : CILsyntax.statement list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmnt) in
    Obj.repr(
# 23 "parser/CILparser.mly"
                  ( (match _1 with | None -> [] | Some e -> [e]) )
# 401 "parser/CILparser.ml"
               : 'stmnts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmnt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmnts) in
    Obj.repr(
# 24 "parser/CILparser.mly"
                  ( (match _1 with | None -> _2 | Some e -> e :: _2) )
# 409 "parser/CILparser.ml"
               : 'stmnts))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmnt) in
    Obj.repr(
# 27 "parser/CILparser.mly"
                                     ( _2 )
# 416 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 30 "parser/CILparser.mly"
                                                                           ( Some (CILTYPE _2) )
# 423 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 31 "parser/CILparser.mly"
                                                                           ( Some (CILTYPEALIAS _2) )
# 430 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 32 "parser/CILparser.mly"
                                                                           ( Some (CILTYPEALIASACTUAL(_2, read_path(_3))) )
# 438 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'names) in
    Obj.repr(
# 33 "parser/CILparser.mly"
                                                                           ( Some (CILCOMMON(_2, _4)) )
# 446 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    Obj.repr(
# 34 "parser/CILparser.mly"
                                                                           ( Some (CILCOMMON(_2, [])) )
# 453 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 35 "parser/CILparser.mly"
                                                                           ( Some (CILCLASSCOMMON(read_path(_2), read_path(_3))) )
# 461 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'names) in
    Obj.repr(
# 36 "parser/CILparser.mly"
                                                                           ( Some (CILCLASS(_2, _4)) )
# 469 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    Obj.repr(
# 37 "parser/CILparser.mly"
                                                                           ( Some (CILCLASS(_2, [])) )
# 476 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 38 "parser/CILparser.mly"
                                                                           ( Some (CILCLASSPERMISSION _2) )
# 483 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'names) in
    Obj.repr(
# 39 "parser/CILparser.mly"
                                                                           ( Some (CILCLASSPERMISSIONSET(read_path(_2), read_path(_4), (Permissions _6))) )
# 492 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'attributeexp) in
    Obj.repr(
# 40 "parser/CILparser.mly"
                                                                           ( Some (CILCLASSPERMISSIONSET(read_path(_2), read_path(_4), (Expression _5))) )
# 501 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'names) in
    Obj.repr(
# 41 "parser/CILparser.mly"
                                                                           ( Some (CILCLASSMAP(_2, _4)) )
# 509 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'names) in
    Obj.repr(
# 42 "parser/CILparser.mly"
                                                                           ( Some (CILCLASSMAPPING(read_path(_2), read_path(_3), (Anonym(read_path(_5), (Permissions _7))))) )
# 519 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'attributeexp) in
    Obj.repr(
# 43 "parser/CILparser.mly"
                                                                           ( Some (CILCLASSMAPPING(read_path(_2), read_path(_3), (Anonym(read_path(_5), (Expression _6))))) )
# 529 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 44 "parser/CILparser.mly"
                                                                           ( Some (CILCLASSMAPPING(read_path(_2), read_path(_3), (Name (read_path(_4))))) )
# 538 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 45 "parser/CILparser.mly"
                                                                           ( Some (CILATTRIBUTE _2) )
# 545 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributeexp) in
    Obj.repr(
# 46 "parser/CILparser.mly"
                                                                           ( Some (CILATTRIBUTESET(read_path(_2), _3)) )
# 553 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmnts) in
    Obj.repr(
# 47 "parser/CILparser.mly"
                                                                           ( Some (CILBLOCK(_2, _3)) )
# 561 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 48 "parser/CILparser.mly"
                                                                           ( Some (CILBLOCKINHERIT(read_path(_2))) )
# 568 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 49 "parser/CILparser.mly"
                                                                           ( Some (CILBLOCKABSTRACT) )
# 575 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmnts) in
    Obj.repr(
# 50 "parser/CILparser.mly"
                                                                           ( Some (CILIN(read_path(_2), _3)) )
# 583 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'formparams) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmnts) in
    Obj.repr(
# 51 "parser/CILparser.mly"
                                                                           ( Some (CILMACRO(_2, _4, _6)) )
# 592 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmnts) in
    Obj.repr(
# 52 "parser/CILparser.mly"
                                                                           ( Some (CILMACRO(_2, [], _5)) )
# 600 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'names) in
    Obj.repr(
# 53 "parser/CILparser.mly"
                                                                           ( Some (CILCALL(read_path(_2), (List.map (fun str -> read_path str) _4))) )
# 608 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 54 "parser/CILparser.mly"
                                                                           ( Some (CILCALL(read_path(_2), [])) )
# 615 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'names) in
    Obj.repr(
# 55 "parser/CILparser.mly"
                                                                           ( Some (CILALLOW(read_path(_2), read_path(_3), Anonym(read_path(_5), (Permissions _7)))) )
# 625 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'attributeexp) in
    Obj.repr(
# 56 "parser/CILparser.mly"
                                                                           ( Some (CILALLOW(read_path(_2), read_path(_3), Anonym(read_path(_5), (Expression _6)))) )
# 635 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 57 "parser/CILparser.mly"
                                                                           ( Some (CILALLOW(read_path(_2), read_path(_3), Name(read_path(_4)))) )
# 644 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'names) in
    Obj.repr(
# 58 "parser/CILparser.mly"
                                                                           ( Some (CILALLOW(read_path(_2), read_path(_2), Anonym(read_path(_5), (Permissions _7)))) )
# 653 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'attributeexp) in
    Obj.repr(
# 59 "parser/CILparser.mly"
                                                                           ( Some (CILALLOW(read_path(_2), read_path(_2), Anonym(read_path(_5), (Expression _6)))) )
# 662 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 60 "parser/CILparser.mly"
                                                                           ( Some (CILALLOW(read_path(_2), read_path(_2), Name(read_path(_4)))) )
# 670 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser/CILparser.mly"
                                                                           ( None )
# 677 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'everys) in
    Obj.repr(
# 62 "parser/CILparser.mly"
                                                                           ( None )
# 685 "parser/CILparser.ml"
               : 'stmnt))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser/CILparser.mly"
                                                 ( A_NAME ["all"] )
# 691 "parser/CILparser.ml"
               : 'attributeexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 66 "parser/CILparser.mly"
                                                 ( A_NAME (read_path(_2)) )
# 698 "parser/CILparser.ml"
               : 'attributeexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'names) in
    Obj.repr(
# 67 "parser/CILparser.mly"
                                                 ( (List.fold_right
                                                      (fun n e -> A_OR(A_NAME (read_path n), e))
                                                      _3
                                                      (A_NAME (read_path(_2)))) )
# 709 "parser/CILparser.ml"
               : 'attributeexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'attributeexp) in
    Obj.repr(
# 71 "parser/CILparser.mly"
                                                 ( A_NOT _3 )
# 716 "parser/CILparser.ml"
               : 'attributeexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'attributeexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'attributeexp) in
    Obj.repr(
# 72 "parser/CILparser.mly"
                                                 ( A_AND(_3, _4) )
# 724 "parser/CILparser.ml"
               : 'attributeexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'attributeexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'attributeexp) in
    Obj.repr(
# 73 "parser/CILparser.mly"
                                                 ( A_OR(_3, _4) )
# 732 "parser/CILparser.ml"
               : 'attributeexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'attributeexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'attributeexp) in
    Obj.repr(
# 74 "parser/CILparser.mly"
                                                 ( A_XOR(_3, _4) )
# 740 "parser/CILparser.ml"
               : 'attributeexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser/CILparser.mly"
                                                 ( A_NAME (read_path(_1)) )
# 747 "parser/CILparser.ml"
               : 'attributeexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser/CILparser.mly"
                          ( )
# 753 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser/CILparser.mly"
                          ( )
# 759 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser/CILparser.mly"
                          ( )
# 765 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser/CILparser.mly"
                          ( )
# 771 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser/CILparser.mly"
                          ( )
# 777 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser/CILparser.mly"
                          ( )
# 783 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser/CILparser.mly"
                          ( )
# 789 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser/CILparser.mly"
                          ( )
# 795 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser/CILparser.mly"
                          ( )
# 801 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser/CILparser.mly"
                          ( )
# 807 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser/CILparser.mly"
                          ( )
# 813 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser/CILparser.mly"
                          ( )
# 819 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser/CILparser.mly"
                          ( )
# 825 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser/CILparser.mly"
                          ( )
# 831 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser/CILparser.mly"
                          ( )
# 837 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser/CILparser.mly"
                          ( )
# 843 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser/CILparser.mly"
                          ( )
# 849 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser/CILparser.mly"
                          ( )
# 855 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser/CILparser.mly"
                          ( )
# 861 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser/CILparser.mly"
                          ( )
# 867 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser/CILparser.mly"
                          ( )
# 873 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser/CILparser.mly"
                          ( )
# 879 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser/CILparser.mly"
                          ( )
# 885 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser/CILparser.mly"
                          ( )
# 891 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser/CILparser.mly"
                          ( )
# 897 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "parser/CILparser.mly"
                          ( )
# 904 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser/CILparser.mly"
                          ( )
# 910 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'everys) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'everys) in
    Obj.repr(
# 105 "parser/CILparser.mly"
                          ( )
# 918 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'everys) in
    Obj.repr(
# 106 "parser/CILparser.mly"
                          ( )
# 925 "parser/CILparser.ml"
               : 'everys))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser/CILparser.mly"
                          ( "in" )
# 931 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser/CILparser.mly"
                          ( "common" )
# 937 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser/CILparser.mly"
                          ( "classcommon" )
# 943 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser/CILparser.mly"
                          ( "class" )
# 949 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser/CILparser.mly"
                          ( "classpermission" )
# 955 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser/CILparser.mly"
                          ( "classpermissionset" )
# 961 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser/CILparser.mly"
                          ( "classmap" )
# 967 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser/CILparser.mly"
                          ( "classmapping" )
# 973 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser/CILparser.mly"
                          ( "block" )
# 979 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser/CILparser.mly"
                          ( "blockabstract" )
# 985 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser/CILparser.mly"
                          ( "type" )
# 991 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser/CILparser.mly"
                          ( "typealias" )
# 997 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser/CILparser.mly"
                          ( "typealiasactual" )
# 1003 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser/CILparser.mly"
                          ( "blockinherit" )
# 1009 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser/CILparser.mly"
                          ( "macro" )
# 1015 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser/CILparser.mly"
                          ( "call" )
# 1021 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser/CILparser.mly"
                          ( "allow" )
# 1027 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser/CILparser.mly"
                          ( "attribute" )
# 1033 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser/CILparser.mly"
                          ( "attributeset" )
# 1039 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser/CILparser.mly"
                          ( "all" )
# 1045 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser/CILparser.mly"
                          ( _1 )
# 1052 "parser/CILparser.ml"
               : 'name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 132 "parser/CILparser.mly"
                ( [_1] )
# 1059 "parser/CILparser.ml"
               : 'names))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'names) in
    Obj.repr(
# 133 "parser/CILparser.mly"
                ( _1 :: _2 )
# 1067 "parser/CILparser.ml"
               : 'names))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 136 "parser/CILparser.mly"
                                                   ( [(PARTYPE, _3)] )
# 1074 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 137 "parser/CILparser.mly"
                                                   ( [(PARCLASS, _3)] )
# 1081 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 138 "parser/CILparser.mly"
                                                   ( [(PARTYPEALIAS, _3)] )
# 1088 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 139 "parser/CILparser.mly"
                                                   ( [(PARCLASSPERMISSION, _3)] )
# 1095 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 140 "parser/CILparser.mly"
                                                   ( [(PARCLASSMAP, _3)] )
# 1102 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 141 "parser/CILparser.mly"
                                                   ( [(PARIGNORE, _3)] )
# 1110 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'formparams) in
    Obj.repr(
# 142 "parser/CILparser.mly"
                                                   ( (PARTYPE, _3) :: _5 )
# 1118 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'formparams) in
    Obj.repr(
# 143 "parser/CILparser.mly"
                                                   ( (PARCLASS, _3) :: _5 )
# 1126 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'formparams) in
    Obj.repr(
# 144 "parser/CILparser.mly"
                                                   ( (PARTYPEALIAS, _3) :: _5  )
# 1134 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'formparams) in
    Obj.repr(
# 145 "parser/CILparser.mly"
                                                   ( (PARCLASSPERMISSION, _3) :: _5  )
# 1142 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'formparams) in
    Obj.repr(
# 146 "parser/CILparser.mly"
                                                   ( (PARCLASSMAP, _3) :: _5  )
# 1150 "parser/CILparser.ml"
               : 'formparams))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'formparams) in
    Obj.repr(
# 147 "parser/CILparser.mly"
                                                   ( (PARIGNORE, _3) :: _5 )
# 1159 "parser/CILparser.ml"
               : 'formparams))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : CILsyntax.statement list)
;;

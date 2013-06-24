type token =
  | LLParen
  | LRParen
  | LPlus
  | LTimes
  | LSkip
  | LAssign
  | LSemi
  | LIf
  | LWhile
  | LEof
  | LInt of (int)
  | LVar of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.stmt

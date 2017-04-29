type token =
  | EOF
  | VAR of (string)
  | STRING of (string)
  | INT of (int)
  | BIN_MULT of (string)
  | PLUS of (string)
  | MINUS of (string)
  | BIN_CMP of (string)
  | NOT
  | AND
  | OR
  | MMINUS
  | LET
  | EQUALS
  | IN
  | CASE
  | OF
  | PIPE
  | GIVES
  | LPAR
  | RPAR

val terminated_expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.t

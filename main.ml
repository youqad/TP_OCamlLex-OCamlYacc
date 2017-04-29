(** Parse expression from the given lexing buffer,
  * return the parsed expression. *)
let parse_expr lexbuf =
  let perror msg =
    let start = lexbuf.Lexing.lex_curr_p in
      Format.printf "Line %d, char %d"
        start.Lexing.pos_lnum
        (1+start.Lexing.pos_cnum-start.Lexing.pos_bol) ;
      if lexbuf.Lexing.lex_curr_pos - lexbuf.Lexing.lex_start_pos > 0 then
        Format.printf ", before %S" (Lexing.lexeme lexbuf) ;
      Format.printf ": %s@." msg ;
      exit 1
  in
    try
      Parse.terminated_expr Lex.token lexbuf
    with
      | Failure s when s = "lexing: empty token" ->
          begin try
            perror
              (Printf.sprintf
                 "empty token before char %c"
                 (Lexing.lexeme_char lexbuf 0))
          with Invalid_argument _ ->
            perror "empty token at end of input"
          end
      | Failure s -> perror s
      | Parsing.Parse_error -> perror "parse error"

let () =
  let lexbuf =
    if Array.length Sys.argv = 1 then
      Lexing.from_channel stdin
    else
      Lexing.from_string Sys.argv.(1)
  in
  let expr = parse_expr lexbuf in
    Format.printf "@[<3>>> %a@]@." Expr.pp_expr expr

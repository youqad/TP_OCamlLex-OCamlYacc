
{
  open Parse   (* ./parse.mly *)
  open Lexing  (* librairie standard *)
}

let varname = ['a'-'z' '_'](['a'-'z' 'A'-'Z' '0'-'9' '_' '\''])*
let int = ['0'-'9']['0'-'9' '_']* | '0'['x' 'X']['a'-'f' 'A'-'F' '0'-'9']['a'-'f' 'A'-'F' '0'-'9' '_']*
let string = '"'([^'"']|'\"')*'"'
let op_cmp = ['>' '=' '<' '^']
let op_mult = ['*' '/' '%']
let op_other = ['!' '$' '?' '.' ':' ';']
let remainder = (op_cmp | op_mult |op_other)*

rule token = parse

  | "case"                  { CASE }
  | "of"                    { OF }
  | "=>"                    { GIVES }
  | "|"                     { PIPE }
  | "let"                   { LET }
  | "="                     { EQUALS }
  | "in"                    { IN }

  | "("                     { LPAR }
  | ")"                     { RPAR }

  | "+" remainder as o      { PLUS o }
  | "-" remainder as o      { MINUS o }
  | op_mult remainder as o  { BIN_MULT  o }
  | op_cmp remainder  as o  { BIN_CMP o }

  | "and"                   { AND }
  | "or"                    { OR }
  | "not"                   { NOT }

  | varname as v            { VAR v }
  | string as s             { STRING s }
  | int as n                { INT (int_of_string n) }

  | [' ' '\t']+   { token lexbuf }
  | ['\r' '\n'] | "\r\n"   { lexbuf.lex_curr_p
                             <- { lexbuf.lex_curr_p with
                                                    pos_bol = lexbuf.lex_curr_p.pos_cnum ;
                                                    pos_lnum = 1 +
                                                               lexbuf.lex_curr_p.pos_lnum };
                             token lexbuf }
  | eof                     { EOF }
  | "(*"                   { comment_section 1 lexbuf }

and comment_section num = parse
  | "(*"                   { comment_section (num+1) lexbuf }
  | "*)"                   { if num>1 then comment_section (num-1) lexbuf
                             else token lexbuf }
  | ['\r' '\n'] | "\r\n"   { lexbuf.lex_curr_p
                             <- { lexbuf.lex_curr_p with
                                                    pos_bol = lexbuf.lex_curr_p.pos_cnum ;
                                                    pos_lnum = 1 +
                                                               lexbuf.lex_curr_p.pos_lnum };
                             comment_section num lexbuf }
  | eof                    { failwith "Nested comments are not closed"}
  | _                      { comment_section num lexbuf }

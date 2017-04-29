%{
    open Expr
%}

%token EOF

%token <string> VAR
%token <string> STRING
%token <int> INT

%token <string> BIN_MULT
%token <string> PLUS
%token <string> MINUS
%token <string> BIN_CMP
%token NOT
%token AND
%token OR
%token MMINUS


%token LET EQUALS IN
%token CASE OF PIPE GIVES

%token LPAR RPAR

/* Les non-terminaux par lesquels l'analyse peut commencer,
* et la donnee de leurs types. */

%right CASE OF PIPE
%nonassoc GIVES
%nonassoc IN

%left OR
%left AND
%left NOT
%left BIN_CMP EQUALS

%left PLUS MINUS
%left BIN_MULT

%nonassoc MMINUS

%start terminated_expr
%type <Expr.t> terminated_expr

%%

terminated_expr:
  | expr EOF { $1 }

expr:
  | const                          { $1 }
  | VAR                            { Var $1 }
  | LPAR expr RPAR                 { $2 }
  | CASE expr OF pmatch            { Case ($2,List.rev $4) }
  | LET VAR EQUALS expr IN expr    { Let ($2,$4,$6) }
  | expr PLUS expr                 { match ($1, $3) with
                                     | (Int n, Int m) -> Int (n+m)
                                     | _ -> App ("+",[$1;$3]) }
  | expr MINUS expr                { match ($1, $3) with
                                     | (Int n, Int m) -> Int (n-m)
                                     | _ -> App ("-",[$1;$3]) }
  | MINUS expr %prec MMINUS        { match $2 with
                                     | Int n -> Int (-n)
                                     | _ -> App ("-",[$2]) }

  | expr BIN_MULT expr             { let op_list = ["*", ( * ); "/", (/); "%", (mod)] in
                                     match ($1, $3) with
                                     | (Int n, Int m) when (try
                                                              let _ = List.assoc $2 op_list in
                                                              true
                                                            with Not_found -> false)
                                       -> let op = List.assoc $2 op_list in
                                          Int (op n m)
                                     | (Int n, Int m) when $2="/" -> Int (n/m)
                                     | (Int n, Int m) when $2="%" -> Int (n mod m)
                                     | _ -> App ($2,[$1;$3]) }
  | expr BIN_CMP expr              { App ($2,[$1;$3]) }
  | expr EQUALS expr               { App ("=",[$1;$3]) }
  | expr AND expr                  { App ("and",[$1;$3]) }
  | expr OR expr                   { App ("or",[$1;$3]) }
  | NOT expr                       { App ("not",[$2]) }

pmatch:
  | prule                          { [$1] }
  | pmatch PIPE prule              { $3::$1 }

prule:
  | pat GIVES expr                 { ($1,$3) }

pat:
  | VAR                            { if $1="_" then Any else Var $1 }
  | const                          { $1 }

const:
  | INT                            { Int $1 }
  | STRING                         { String $1 }

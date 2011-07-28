%{
 open Latex
 (*open Prelude*)
%}

%token EOL
%token <Latex.t> SYMB

%token COMMA SEMICOLON
%token METAPARENL METAPARENR 
%token PARENL PARENR BRACKETL BRACKETR BRACEL BRACER

%token POINTYL POINTYR BAR

%token REDUCES

%token SUB SUP

%token MU

%start <Latex.t> mu

%%

mu:
| e=expr EOL { mode M e }

expr:
| PARENL e=expr PARENR { concat [text"(";e;text")"] }
| METAPARENL e=expr METAPARENR { e }
| s=SYMB { s }
| MU p=pattern COMMA e=expr { concat [Latex.mu;p;text",";e] }

pattern:
| e=expr { e }

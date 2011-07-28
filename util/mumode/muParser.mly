%{
 open Latex
 (*open Prelude*)
%}

%token EOL
%token <Latex.t> SYMB

%token COMMA SEMICOLON
%token METAPARENL METAPARENR 
%token PARENL PARENR BRACKETL BRACKETR BRACEL BRACER

%token MU

%start <Latex.t> mu

%%

mu:
| e=expr EOL { mode M e }

expr:
| s=SYMB { s }
| MU p=pattern COMMA e=expr { concat [Latex.mu;p;text",";e] }

pattern:
| e=expr { e }

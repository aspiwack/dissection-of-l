%{
 open Latex
 open Prelude

 (*let invamp = command "bindnasrepma" ~packages:["stmaryrd",""] [] M*)

let ulcorner = command "ulcorner" ~packages:["amssymb",""] [] M
let urcorner = command "urcorner" ~packages:["amssymb",""] [] M
let llcorner = command "llcorner" ~packages:["amssymb",""] [] M
let lrcorner = command "lrcorner" ~packages:["amssymb",""] [] M

let empty_context = Latex.cdot
%}

%token EOL
%token <Latex.t> SYMB

%token COMMA SEMICOLON COLON
%token METAPARENL METAPARENR 
%token PARENL PARENR BRACKETL BRACKETR BRACEL BRACER BRACEBR
%token WILDCARD
%token TURNSTYLE TURNSTYLEV VEC

%token POINTYL POINTYR BAR
%token DUAL
%token OPLUS OTIMES WITH PAR
%token LARROW
%token BANG WHYNOT
%token SHIFTP SHIFTN
%token PI SIGMA

%token IOTA1 IOTA2
%token PI1 PI2 FIELD1 FIELD2
%token LLCORNER LRCORNER
%token LAMBDA
%token VAL

%token REDUCES
%token SUBST

%token SUB SUP

%token MU MUT

%token <Latex.t->Latex.t->Latex.t->Latex.t> FUN3

%start <Latex.t> mu

%right REDUCES
%nonassoc MU
%right APP
%nonassoc SUBSUP

%%

mu:
| e=expr EOL { mode M e }
| s=sequent EOL {mode M s }

expr:
| PARENL e=expr PARENR { concat [text"(";e;text")"] }
| METAPARENL e=expr METAPARENR { e }
| s=SYMB { s }
| WILDCARD { text"\\_" }

| f=FUN3 e1=expr e2=expr e3=expr { f e1 e2 e3 } %prec APP

| e=expr SUB s=expr {index e s} %prec SUBSUP
| e=expr SUP s=expr {exponent e s} %prec SUBSUP
| e=expr SUB s1=expr SUP s2=expr {index_exponent e s1 s2} %prec SUBSUP
| e=expr SUP s1=expr SUB s2=expr {index_exponent e s2 s1} %prec SUBSUP

| MU p=pattern COMMA e=expr { concat [Latex.mu;p;text".\\,";e] } %prec MU
| MUT p=pattern COMMA e=expr { concat [tilde Latex.mu;p;text".\\,";e] } %prec MU
| POINTYL e=expr BAR f=expr POINTYR { concat [langle;e;mid;f;rangle] }

| PARENL t=expr COMMA u=expr PARENR { concat [text"(";t;text",";u;text")"] }
| PARENL PARENR { text"()" }
| BRACEL t=expr COMMA u=expr BRACER { concat [text"\\{~";t;text"\\,,\\,";u;text"~\\}"] }
| BRACEBR t=expr COMMA u=expr BRACER { concat [
                                        text"\\{";newline;
                                        phantom alpha; quad;t;text",";newline;
					phantom alpha; quad;u;newline;
                                        text"\\}"
                                      ]}
| BRACEL BRACER { text"\\{\\}" }
| IOTA1 u=expr { concat [text "1." ; u] }
| IOTA2 u=expr { concat [text "2." ; u] }
| u=expr PI1 { concat [u; text".1"] }
| u=expr PI2 { concat [u; text".2"] }
| FIELD1 u=expr { concat [text"1 = "; u] }
| FIELD2 u=expr { concat [text"2 = "; u] }
| LLCORNER u=expr LRCORNER { concat [left `Floor;u;right `Floor] }
| LLCORNER LRCORNER { concat [left `Floor;phantom(text"x");right `Floor] }
| VAL u=expr { just_left (`Double `Down) u }

| LAMBDA p=pattern COMMA e=expr {concat [lambda;p;text".\\,";e] } %prec MU
| t=expr u=expr { concat[t;text"~";u] } %prec APP

| e=expr DUAL { exponent e bot }
| a=expr OTIMES b=expr { concat[a;tensor;b] }
| a=expr OPLUS b=expr  { concat[a;plus;b] }
| a=expr WITH b=expr  { concat[a;withc;b] }
| a=expr PAR b=expr  { concat[a;parr;b] }
| BANG a=expr { concat [text"\\,!";a] }
| WHYNOT a=expr { concat [text"\\,?\\!";a] }
| SHIFTP a = expr { concat [text"\\,";downarrow;text"\\!";a] }
| SHIFTN a = expr { concat [text"\\,";uparrow;text"\\!";a] }
| PI p=typedpattern COMMA b=expr { concat [ index prod p ; b ] }
| SIGMA p=typedpattern COMMA b=expr { concat [ index sum p ; b ] }

| a=expr LARROW b=expr { concat [a;larrow;b] }

| SUBST l=delimited(BRACKETL, separated_nonempty_list(SEMICOLON,separated_pair(expr,COMMA,expr)) ,BRACKETR) e=expr
  { let single (t,x) = concat[x;setminus;t] in
    let subst =
      match l with
      | s::r ->
          single s ^^ concat (List.map (fun s -> text", "^^single s) r)
      | [] -> assert false
    in
    concat [e;text"[";subst;text"]"]
  }
| e=expr REDUCES f=expr { concat [e;leadsto;f] }

typedpattern:
| p = pattern COLON a=expr { concat [ p ; text"{:}" ; a ] }
| VEC p=pattern COLON a=expr { concat [ overline p ; text"{:}" ; overline a ] }

pattern:
| e=expr { e }


nonempty_context:
| c=separated_nonempty_list(COMMA,context_item) {
  List.hd c ^^ Latex.concat (List.map (fun x -> text", "^^x) (List.tl c))
}
context:
| c=nonempty_context { c }
|   { empty_context }

context_item:
| p=typedpattern { p }
| s=expr { s }

sequent_right_hand:
| e=expr { e }
| t=expr COLON a=expr { concat [ t ; text" : " ; a ] }

sequent_left_hand:
| g=context SEMICOLON gs=separated_nonempty_list(SEMICOLON,context) {
  g ^^ Latex.concat (List.map (fun x-> text"~;~"^^x) gs)
}
| c=nonempty_context { c }
| { Latex.empty }

sequent:
| l=sequent_left_hand TURNSTYLE r=sequent_right_hand {
  concat [ l ; vdash ; r ] }
| l=sequent_left_hand TURNSTYLEV r=sequent_right_hand {
  concat [ l ; vdash ; text"_" ; text"v" ; r ] }


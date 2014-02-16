open MuParser
open Latex
open Prelude

(*** Formatting primitive ***)

let keyword k = SYMB (textbf (text k))
let longvar x = SYMB (mathrm (text x))
let number n = SYMB (mode M (text n))

(*** Global tables ***)

let keywords = []
let map_ident = [
  "vec" , VEC ;

  "mu" , MU ;
  "mut" , MUT ;
  "lambda" , LAMBDA ;
  "PI" , PI ;
  "SIGMA" , SIGMA ;

  "bot" , SYMB bottom ;
  "top" , SYMB top;

  "val" , VAL ;
  "thunk" , THUNK ;
  "shiftn" , SHIFTN ;
  "shiftp" , SHIFTP ;
  "chain" , FUN3 (fun m x n -> concat[m;mathrm (text"\\,to\\,");x;text".\\,";n]) ;

  "subst", SUBST;

  "cv" , SYMB bullet ;

  (*greek letters*)
  "alpha" , SYMB alpha ;
  "beta" , SYMB beta ;
  "gamma" , SYMB gamma ;
  "Gamma" , SYMB gamma_;
  "delta" , SYMB delta ;
  "Delta" , SYMB delta_;
  "epsilon" , SYMB varepsilon ;
  "theta" , SYMB theta ;
  "Theta" , SYMB theta_;
  "kappa" , SYMB kappa ;
  "Pi" , SYMB pi_;
  "rho" , SYMB rho ;
  "Sigma" , SYMB sigma_;
  "phi" , SYMB varphi ;
  "psi" , SYMB psi ;
  "Psi" , SYMB psi_;
  "Xi" , SYMB xi_;
  "Omega", SYMB omega_;
]

let regexp whitespace = [ ' '  '\t' '\n' '\r' ]

let regexp lower_case = ['a'-'z']
let regexp upper_case = ['A'-'Z']
let regexp letter = (lower_case | upper_case)
let regexp digit = ['0'-'9']

let regexp lvar = lower_case['\'']*
let regexp uvar = upper_case['\'']*
let regexp cal_var = 'c' upper_case
let regexp bold_var = 'b' letter
let regexp bb_var = "bb" upper_case
let regexp var_star = upper_case "_*"
let regexp ident = letter+
let regexp number = "-"? digit+

let lexeme = Ulexing.utf8_lexeme


let map = 
  map_ident @
  (List.map (fun k -> k,keyword k) keywords)

let next k = lexer
  | lvar | uvar -> SYMB (mode M (text (lexeme lexbuf)))
  | ident ->
    let id = lexeme lexbuf in
    begin try 
	 List.assoc id map
      with Not_found ->
	longvar id
    end

  | "'o'" -> COMP

  | "~~>" -> REDUCES

  | "<" -> POINTYL | ">" -> POINTYR
  | "|" -> BAR
  | "1." -> IOTA1 | "2." -> IOTA2
  | ".1" -> PI1 | ".2" -> PI2
  | "1=" -> FIELD1 | "2=" -> FIELD2
  | "|_" -> LLCORNER | "_|" -> LRCORNER

  | "\\(" -> METAPARENL | "\\)" -> METAPARENR

  | "|-" -> TURNSTYLE | "|-_v" -> TURNSTYLEV | "|-_p" -> TURNSTYLEP

  | "," -> COMMA | ";" -> SEMICOLON
  | ":" -> COLON
  | "(" -> PARENL | ")" -> PARENR
  | "[" -> BRACKETL | "]" -> BRACKETR
  | "{" -> BRACEL | "}" -> BRACER
  | "{\\\\" -> BRACEBR
  | "_" -> SUB | "^" -> SUP

  | "1" -> SYMB one | "0" -> SYMB zero
  | "<+>" -> OPLUS | "<*>" -> OTIMES
  | "&" -> WITH | "`&" -> PAR
  | "!" -> BANG | "?" -> WHYNOT

  | "-o" -> LARROW

  | "^~" -> DUAL

  | "=" -> EQUAL

  | "\\_" -> WILDCARD

  | whitespace -> k ()
  | number -> number (lexeme lexbuf)

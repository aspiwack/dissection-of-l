open MuParser
open Latex
(*open Prelude*)

(*** Formatting primitive ***)

let keyword k = SYMB (textbf (text k))
let longvar x = SYMB (mathrm (text x))
let number n = SYMB (mode M (text n))

(*** Global tables ***)

let keywords = []
let map_ident = [
  "mu" , MU ;
  "mut" , MUT ;
  "lambda" , LAMBDA ;
  "PI" , PI ;
  "SIGMA" , SIGMA ;

  "subst", FUN3 (fun t x u -> concat [t;text"[";x;setminus;u;text"]"]);

  (*greek letters*)
  "alpha" , SYMB alpha ;
  "beta" , SYMB beta ;
  "kappa" , SYMB kappa ;
  "Gamma" , SYMB gamma_;
  "Delta" , SYMB delta_;
  "Xi" , SYMB xi_;
  "Pi" , SYMB pi_;
  "Sigma" , SYMB sigma_;
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

  | "~~>" -> REDUCES

  | "<" -> POINTYL | ">" -> POINTYR
  | "|" -> BAR
  | "1." -> IOTA1 | "2." -> IOTA2
  | "|_" -> LLCORNER | "_|" -> LRCORNER

  | "\\(" -> METAPARENL | "\\)" -> METAPARENR

  | "|-" -> TURNSTYLE

  | "," -> COMMA | ";" -> SEMICOLON
  | ":" -> COLON
  | "(" -> PARENL | ")" -> PARENR
  | "[" -> BRACKETL | "]" -> BRACKETR
  | "{" -> BRACEL | "}" -> BRACER
  | "{\n" -> BRACEBR
  | "_" -> SUB | "^" -> SUP

  | "<+>" -> OPLUS | "<*>" -> OTIMES
  | "&" -> WITH | "`&" -> PAR
  | "!" -> BANG | "?" -> WHYNOT

  | "^~" -> DUAL

  | "\\_" -> WILDCARD

  | whitespace -> k ()
  | number -> number (lexeme lexbuf)

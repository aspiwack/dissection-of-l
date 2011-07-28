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
  "MU" , MU ;
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

  | "\\(" -> METAPARENL | "\\)" -> METAPARENR

  | "(" -> PARENL | ")" -> PARENR
  | "[" -> BRACKETL | "]" -> BRACKETR
  | "{" -> BRACEL | "}" -> BRACER
  | "_" -> SUB | "^" -> SUP

  | whitespace -> k ()
  | number -> number (lexeme lexbuf)

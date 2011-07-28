(*** Shifts a lexer (of string) to act on a custom-mode input. ***)

module type Lexer = sig
  type t
  type lexbuf

  val init : string -> lexbuf
  val exhausted : lexbuf -> bool

  val lex_one : (unit -> t) -> lexbuf -> t
  val eos : t

  val code : Latex.t -> t
  val math : Latex.t -> t
  val text : Latex.t -> t
end

module Make (L:Lexer) : sig
  type t = L.t
  type lexbuf

  val init : [ `V of string | `C of Latex.t | `M of Latex.t | `T of Latex.t] list
             -> lexbuf

  val next : lexbuf -> t
end
= struct

  type t = L.t

  type lexbuf = {
    mutable string : L.lexbuf option ;
    mutable stream :
      [ `V of string | `C of Latex.t | `M of Latex.t | `T of Latex.t] list
  }

  let init l =
    { string = None;
      stream = l }

  let lexer_next sk fk lexbuf = 
    if L.exhausted lexbuf then
      fk ()
    else
      L.lex_one sk lexbuf

  let quote = function
    | `C x -> L.code x
    | `M x -> L.math x
    | `T x -> L.text x
    | `V x -> assert false

  let rec next lexbuf =
    match lexbuf.string , lexbuf.stream with
    | None , [] -> L.eos
    | None , (`V s) :: l -> 
      lexbuf.string <- Some (L.init s) ; 
      lexbuf.stream <- l ;
      next lexbuf
    | None , x :: l -> 
      lexbuf.stream <- l ;
      quote x
    | Some ulexbuf , l ->
      lexer_next
	(fun () -> next lexbuf)
	(fun () -> lexbuf.string <- None ; next lexbuf)
	ulexbuf
end

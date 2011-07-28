module Lex = Lexquot.Make (struct
  type t = MuParser.token
  type lexbuf = Ulexing.lexbuf

  let init = Ulexing.from_utf8_string

  let exhausted l =
    Ulexing.start l;
    let i = Ulexing.next l in
    if i = -1 then true
    else begin
      ignore (Ulexing.backtrack l);
      false
    end

  let lex_one = MuLexer.next
  let eos = MuParser.EOL

  let code x = MuParser.SYMB x
  let math x = MuParser.SYMB x
  let text x = MuParser.SYMB x
end)

let parse l =
  let parse =
    MenhirLib.Convert.traditional2revised
      (fun x -> x)
      (fun _ -> Lexing.dummy_pos)
      (fun _ -> Lexing.dummy_pos)
      Muparser.mu
  in
  let next =
    let lexbuf = Lex.init l in
    fun () -> Lex.next lexbuf
  in
  parse next

let mumode = parse

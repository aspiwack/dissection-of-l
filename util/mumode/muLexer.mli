open MuParser

val next : (unit -> token) -> Ulexing.lexbuf -> token

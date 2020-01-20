type token = Parser.token * Lexing.position * Lexing.position

val menhir_token : Parser.token -> token

val of_menhir_token : token -> Parser.token

type t

val make : int -> Lexing.lexbuf -> t

val read_token : ?back:int -> t -> token

val next_token : t -> token * t

val position : t -> int

val rewind : t -> t

(**

   The following module provide a persistent representation for the
   lexer states despite the inherently impure nature of Lexing.lexbuf.

   To be precise, the representation is semi-persistent as it only
   supports multiple versions of the lexing state which may only
   differ on a finite number of recent tokens.

*)

(** Menhir's token are decorated with positions.
    We skip this aspect in this POC. *)
type token = Parser.token * Lexing.position * Lexing.position

val menhir_token : Parser.token -> token

val of_menhir_token : token -> Parser.token

(** The semi-persistent lexer state. *)
type t

(** [make d lexbuf] produces a lexer persistent up-to the last [d]
   tokens. *)
val make : int -> Lexing.lexbuf -> t

(** [read_token ~back:n lexbuf] returns the token which was produced
    [n] steps ago. Default value for back is 0. *)
val read_token : ?back:int -> t -> token

(** [next_token lexer] returns the next token and the next state
    for the lexer. *)
val next_token : t -> token * t

(** [position lexer] returns the number of tokens read so far. *)
val position : t -> int

(** [rewind lexer] is the [lexer] one step in the past. *)
val rewind : t -> t

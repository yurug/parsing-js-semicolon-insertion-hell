open Parser.MenhirInterpreter

let ( ||| ) f g =
  fun x ->
  match f x with
  | None -> g x
  | Some y -> Some y

let rec next_reduction lookahead checkpoint =
  match lookahead, checkpoint with
  | _, AboutToReduce (_, production) ->
     Some production
  | Some lookahead, InputNeeded _ ->
     next_reduction None (offer checkpoint lookahead)
  | None, InputNeeded _ ->
     None
  | _, Shifting _ ->
     next_reduction lookahead (resume checkpoint)
  | _, (Accepted _  | HandlingError _ | Rejected) ->
     None

let rec before_next_shift checkpoint =
  match checkpoint with
  | AboutToReduce _ ->
     before_next_shift (resume checkpoint)
  | InputNeeded _ ->
     None
  | Shifting (before, _, _) ->
     Some before
  | (Accepted _  | HandlingError _ | Rejected) ->
     None

let string_of_token = Parser.(function
  | SEMICOLON -> "SEMICOLON"
  | IDENT x -> Printf.sprintf "IDENT(%s)" x
  | DO -> "DO"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EQUAL -> "EQUAL"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | EOF -> "EOF"
  | NEWLINE -> "NEWLINE"
  | BREAK -> "BREAK"
)

type token = Parser.token * Lexing.position * Lexing.position

let menhir_token token = (token, Lexing.dummy_pos, Lexing.dummy_pos)

let of_menhir_token (token, _, _) = token

type proxy = {
            lexbuf    : Lexing.lexbuf;
    mutable token_num : int;
            token_win : token BoundedBuffer.t;
}

type t = {
    proxy   : proxy;
    current : int;
}

let before_start = -1

let make_proxy backtracking_limit lexbuf =
  { lexbuf;
    token_num = before_start;
    token_win = BoundedBuffer.make backtracking_limit;
  }

let make backtracking_limit lexbuf =
  {
    proxy   = make_proxy backtracking_limit lexbuf;
    current = before_start;
  }

let really_get proxy =
  let token = menhir_token (Prelexer.token proxy.lexbuf) in
  BoundedBuffer.push proxy.token_win token;
  proxy.token_num <- proxy.token_num + 1

let read_proxy proxy idx =
  BoundedBuffer.get proxy.token_win idx

let read proxy idx =
  read_proxy proxy (proxy.token_num - idx)

let read_token ?(back = 0) lexer =
  read lexer.proxy (lexer.current - back)

let next_token lexer =
  if lexer.proxy.token_num = lexer.current then really_get lexer.proxy;
  let current = lexer.current + 1 in
  (read lexer.proxy current, { lexer with current })

let rewind lexer =
  { lexer with current = lexer.current - 1 }

let position lexer =
  lexer.current

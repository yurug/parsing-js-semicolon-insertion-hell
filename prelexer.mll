{ (* Emacs, open this with -*- tuareg -*- *)
  open Parser
}

rule token = parse
| " "             { token lexbuf }
| "break"         { BREAK        }
| "do"            { DO           }
| "while"         { WHILE        }
| "for"           { FOR          }
| "="             { EQUAL        }
| "{"             { LBRACE       }
| "}"             { RBRACE       }
| "("             { LPAREN       }
| ")"             { RPAREN       }
| ";"             { SEMICOLON    }
| "\n"            { NEWLINE      }
| eof             { EOF          }
| ['a'-'z']+ as x { IDENT x      }
| _ as c          { failwith (Printf.sprintf "Unexpected character `%c'." c) }

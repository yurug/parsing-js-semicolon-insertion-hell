%{ (* Emacs, open this with -*- tuareg -*- *)

open AST

%}

%token LPAREN RPAREN LBRACE RBRACE SEMICOLON EOF EQUAL DO WHILE BREAK FOR
%token<string> IDENT
%token NEWLINE

%start<AST.goal> goal

%%

goal: s=statement EOF
{
  s
}

statement:
| DO b=block WHILE LPAREN e=expression RPAREN SEMICOLON
{
  DoWhile (b, e)
}
| b=block
{
  b
}
| BREAK (* No Newline here. *) x=IDENT SEMICOLON
{
  Break x
}
| BREAK SEMICOLON
{
  Break ""
}
| x=IDENT EQUAL e=expression SEMICOLON
{
  Assign (x, e)
}
| SEMICOLON
{
  Nop
}
| FOR
  LPAREN a=expression SEMICOLON b=expression SEMICOLON c=expression RPAREN
  s=statement
{
  For (a, b, c, s)
}

expression: x=IDENT
{
  Var x
}

block: LBRACE ss=statement* RBRACE
{
  Block ss
}



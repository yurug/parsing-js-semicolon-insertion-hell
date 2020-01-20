.PHONY: all clean

all:
	dune build semicolon.exe
	ln -sf _build/default/semicolon.exe semicolon

clean:
	dune clean

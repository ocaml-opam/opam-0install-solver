.PHONY: all test

all:
	dune build ./main.exe

test:
	dune exec ./test/test.exe

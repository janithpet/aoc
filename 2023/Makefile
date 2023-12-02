.PHONY: run build clean
CC=ocamlopt

SRC=main.ml
BIN=main
BUILD=build

$(BIN): $(SRC)
	$(CC) $(SRC) -o $(BIN)

PACKAGES = core,base,re

.PHONY : %
day1:
	@cd day1 && make run -B

clean:
	rm -f $(BIN) **.cmi **.cmx **.o

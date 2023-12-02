ENTRYPOINT = _build/default/bin/main.exe
OCAML_SOURCES:= \
	bin \
	lib \

$(ENTRYPOINT): $(shell find $(OCAML_SOURCES) -type f)
	@dune fmt
	@dune build

.PHONY := \
	run \
	build \
	clean

build:
	@dune fmt
	@dune build

run: $(ENTRYPOINT)
	@$^

clean:
	dune clean
	rm -rf $(MODIFIED_SOURCES_LOCATION)
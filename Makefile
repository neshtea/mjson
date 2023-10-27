.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: test
test:
	dune test

.PHONY: repl
repl:
	dune utop --profile release

.PHONY: doc
doc:
	dune build @doc
	dune build @doc-private

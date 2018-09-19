DUNE ?= dune

build:
	$(DUNE) build

doc:
	$(DUNE) build @doc

test:
	$(DUNE) runtest -f

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

clean:
	$(DUNE) clean

.PHONY: build doc test install uninstall clean

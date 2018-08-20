DUNE ?= dune

build:
	$(DUNE) build

doc:
	$(DUNE) build @doc

test:
	$(DUNE) runtest

install:
	@echo 'welp'

clean:
	$(DUNE) clean

.PHONY: build doc test install clean

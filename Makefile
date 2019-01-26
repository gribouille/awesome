.PHONY: build clean test build-doc

build:
	stack build --fast
	
test: build
	stack test

build-doc:
	stack hoogle -- generate --local

doc:
	stack hoogle -- server --local --port=8080

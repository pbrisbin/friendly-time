all: setup setup.lint build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) \
	  --fast --test --no-run-tests --dependencies-only

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) \
	  --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) \
	  --fast --pedantic --test

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint .
	stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: clean
clean:
	stack clean

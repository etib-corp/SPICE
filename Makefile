all: build

build:
	stack build

tests_run:
	stack test --coverage

clean:
	stack clean

.PHONY: all build clean

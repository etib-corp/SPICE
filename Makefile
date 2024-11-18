all: build

build:
	stack build

tests_run:
	stack test

clean:
	stack clean

.PHONY: all build clean

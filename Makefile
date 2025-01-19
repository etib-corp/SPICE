NAME = glados

all: build

build:
	stack build
	cp $(shell stack path --local-install-root)/bin/spice-exe ./$(NAME)

tests_run:
	stack test --coverage

clean:
	stack clean

fclean: clean
		rm -f $(NAME)

re: clean all

.PHONY: all build clean

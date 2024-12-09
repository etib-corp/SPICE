# Usage
> Build and preview a site locally

Make sure to run all commands from the `root` directory, as that is where `Makefile` is.

```sh
$ cd SPICE
```


## CLI help

```sh
$ ./glados --help   // Integration in progress
```
or

```sh
$ ./glados -h   // Integration in progress
```

<!-- ```
default: install
all: install build
h help:
install:
upgrade:
s serve:
b build:
d deploy:
``` -->


## DOCS

Please check the main repository

```
https://github.com/etib-corp/SPICE?tab=readme-ov-file#readme
```

<!-- Then open in your browser:

- [localhost:8000](http://localhost:8000) -->


## Build SPICE

Build SPICE compiler.

```sh
$ make
```
or
```sh
$ make build
```

## Usage


Use this project in two ways: read from a file or interact directly via prompt input

```sh
$ ./glados < foo.scm
```
or

```sh
> foo
*** ERROR : variable foo is not bound.
> (define foo 42)
> foo
42
```


## SPICE Features


Function declaration :

```
(define add
(lambda (a b)
(+ a b)))
(add 3 4)
```


Builtin's function :

```
(+ (* 2 3) (div 10 2))
(* (* 2 3) (div 10 2))
(- (* 2 3) (div 10 2))
(eq (* 2 3) (div 10 2))
(< (* 2 3) (div 10 2))
(div (* 2 3) (div 10 2))
```

Conditional jump:

```
(if #t 1 2)
```

## Clean SPICE repository

Clean repository.

```sh
$ make clean
```

# Usage
> Build and preview a site locally

Make sure to run all commands from the `root` directory, as that is where `Makefile` is.

```sh
$ cd SPICE
```


## CLI help

```sh
$ ./glados --help
Usage: optparse-app [-v|--verbose] [-c|--config FILE] [FILES...] [-e|--execute]
                    [-C|--compile] [-P|--preprocess]

Available options:
  -v,--verbose             Verbose output
  -c,--config FILE         Configuration file
  -e,--execute             Execute the program
  -C,--compile             Compile the program
  -P,--preprocess          Preprocess the program
  -h,--help                Show this help text
```
or

```sh
$ ./glados -h
Usage: optparse-app [-v|--verbose] [-c|--config FILE] [FILES...] [-e|--execute]
                    [-C|--compile] [-P|--preprocess]

Available options:
  -v,--verbose             Verbose output
  -c,--config FILE         Configuration file
  -e,--execute             Execute the program
  -C,--compile             Compile the program
  -P,--preprocess          Preprocess the program
  -h,--help                Show this help text
```

## DOCS

Please check the main repository

```
https://github.com/etib-corp/SPICE?tab=readme-ov-file#readme
```


## Build SPICE

Build SPICE compiler.

```sh
$ make
```
or
```sh
$ make build
```

## Usage of Basic Interpreter


Use this project in two ways: read from a file or interact directly via prompt input

```sh
$ ./glados < foo.scm
```
or

```sh
$ ./glados
> foo
*** ERROR : variable foo is not bound.
> (define foo 42)
> foo
42
```

## Usage of Spice Compiler

```sh
$ ./glados [-c|--config] config.js [-C|--compile] your_source_code.ext
```

The resulting file is out.bin by default. It contains the bytecode of the compiled source code

```sh
$ ls
out.bin
```

You can use [-P|--preprocess] to switch in preprocessing mode

```sh
$ ./glados [-c|--config] config.js [-C|--compile] your_source_code.ext [-P|--preprocess]
[]
[]
[CallFunc "facto" [[PushInt 5]],Print]
[22,102,97,99,116,111,0,4,0,1,5,0,255,21]
```

## Usage of Spice VM (Virtual Machine)

```sh
$ ./glados [-e|--execute] out.bin
...
```

## SPICE Interpreter Features


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

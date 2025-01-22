# Advanced
> Beyond the basic configuration and content

Once you've got the basics of compiler functionality down, you can explore deeper using this guide to understand its key components: the frontend, AST, middlend, and backend.

## FRONTEND COMPILER

The frontend is responsible for parsing source code and converting it into an intermediate representation. This includes:

- **Lexical Analysis**: Tokenizing the input code into symbols and keywords.
- **Syntax Analysis**: Creating a syntax tree based on grammar rules.
- **Semantic Analysis**: Checking for type consistency, scope validation, and other rules.

For example, this step generates an Abstract Syntax Tree (AST), representing the code's structure.

```yaml
Frontend:
  - Lexical Analysis
  - Syntax Analysis
  - Semantic Analysis
  - Output: AST
```

The frontend must be defined using a configuration file that describes the grammar of the target source language.

An example of lisp and js config file

Config file for JS

```sh
boolean{prefix:"(", suffix:")"}: ["true", "false"]
variable{prefix:"", suffix: ""}: ["let", "const", "var"]
operators: [plus: expression -> "+" -> expression , minus: expression -> "-" -> expression , multiply: expression -> "*" -> expression , divide: expression -> "/" -> expression , modulo: expression -> "%" -> expression , equal: expression -> "===" -> expression , assignation: expression -> "=" -> expression, less: expression -> "<" -> expression, greater: expression -> ">" -> expression]
condition{prefix: "(", suffix: ")"}: expression
parameters{prefix: "(", suffix: ")"}: name -> ","
codeBlock{prefix: "{", suffix: "}"}: ["\n",";"]
if: ["if"] -> ["else"]
function: ["function"] -> name -> parameters -> codeBlock
callable: ["("] -> "," -> [")"]
```

Config file for Lisp

```sh
boolean{prefix: "(", suffix: ")"}: ["#t", "#f"]
variable{prefix: "(", suffix: ")"}: ["define"]
operators{prefix: "(", suffix: ")"}: [plus: "+" -> expression -> expression, minus: "-" -> expression -> expression, multiply: "*" -> expression -> expression, divide: "/" -> expression -> expression, modulo: "%" -> expression -> expression, equal: "eq?" -> expression -> expression, assignation: name -> "=" -> expression]
condition{prefix: "(", suffix: ")"}: expression
parameters{prefix: "(", suffix: ")"}: name -> " "
codeBlock{prefix: "(", suffix: ")"}: [""]
if{prefix: "(", suffix: ")"}: ["if"] -> [""]
function{prefix: "(", suffix: ")"}: ["define"] -> name -> parameters -> codeBlock
```

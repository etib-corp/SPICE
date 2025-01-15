# Create your own programming language using SPICE

## Introduction

With SPICE you can create your own language. This is done by creating a parser configuration file.
This configuration file respects our own syntax and is used to define the syntax of your language.

## Syntax

### Links between expressions

You can link expressions together by using the `->` operator. This operator is used to tell the parser that the expression on the left side is followed by the expression on the right side.

Here is how you can use the `->` operator:

- `expression -> expression`: Define that the expression on the left side is followed by the expression on the right side.

### Formatters

Each type of expression can have a `prefix` and/or a `suffix` that will be used to define the syntax of your language.

Here is how you can define a `prefix` and a `suffix`:
- Only a `prefix`:
    - `key{prefix: "..."}: value`.
- Only a `suffix`:
    - `key{suffix: "..."}: value`.
- Both a `prefix` and a `suffix`:
    - `key{prefix: "...", suffix: "..."}: value`.

### Values

Keywords are used to define the syntax of your language. They are used to define the structure of your language.

Here are the keywords that you can use:

- `expression`: Define an expression, it is used to tell the parser *"Hey ! Here please parse an expression"*.
- `name`: Define a name, it is used to tell the parser *"Hey ! Here please parse a name"*. The name is a string of characters, starting with a letter and followed by letters or digits.
- `many(expression)`: Define a list of expression.
- `["...",...]`: Define a list of string that can be parsed.

### Keys

Key and values are used to define the syntax of your language. They are used to define the structure of your language.

Here are the key and values that you **have to** define:

- `boolean`: Define a boolean, it is used to tell the parser *"Hey ! Here is how you have to parse a boolean."*.
It must be define as a `list of string` that can be parsed, this list must contain 2 values.
    - *Example:* `boolean: ["true", "false"]`.
- `variable`: Define a variable, it is used to tell the parser *"Hey ! Here is how you have to parse a variable."*.
It must be define as a `list of string` followed by a `name`, or a `name` followed by a `list of string`.
    - *Example:* `variable: ["var", "let", "const"] -> name`.
- `operators`: Define a list of operators, it is used to tell the parser *"Hey ! Here is how you have to parse operators."*.
It must contain a list of key/values, where the key is the operator and the value is the precedence of the operator.
    - *Example:* `operators: [plus: expression -> "+" -> expression, minus: "-" -> expression -> expression, ...]`.
    - **Note:**
        - You **_have to_** define these operators:
            - `plus` which is usualy `+`.
            - `minus` which is usualy `-`.
            - `multiply` which is usualy `*`.
            - `divide` which is usualy `/`.
            - `modulo` which is usualy `%`.
            - `assignation` which is usualy `=`.
            - `equal` which is usualy `==`.
            - `greater` which is usualy `>`.
            - `less` which is usualy `<`.
        - An `operator` as to be defined with `2 expressions` and `1 string`.
            - *Example:* `plus: "+" -> expression -> expression`
- `condition`: Define condition's syntax, here it is very strict: you only have to modify the [formatters](#formatters).
Please do not define another syntax than the following given:
    - *Example:* `condition{your_formatters}: expression`
- `parameters`: Define parameters syntax, here is another format and be careful here. Parameters has 2 elements: a `name` and a `string`.
    - Here you have to define the separator, so just only the `string`.
    - **Warning:** The first character of the `string` must not be a digit or a letter.
    - *Example:* `parameters: name -> " toto "`.
- `codeBlock`: Define code block's syntax, as condition, it is very strict here, so you only have to modify the [formatters](#formatters).
Please do not define another syntax than the following given:
    - *Example:* `codeBlock{your_formatters}: many(expression)`
- `if`: Define an if statement. Here is another format too, an if key contains `2 list of strings`:
the first one define the first statement ("if" for example) and the second one the last statement ("else" for example).
    - *Example:* `if: ["if", "imagine"] -> ["else", "back to reality"]`
- `function`: Define a function syntax. Firstly, we wanted that you can define any syntax that you want, but it enables the creation of anarchic syntaxes !
    - A function syntax contains:
        - A `list of string` which corresponds to the declarator(s) that can define a function.
        - A `name` which corresponds to the name of the function.
        - A `parameters` which corresponds to the parameters of the function. (It tells the parser *"Hey ! Parse parameters as I showed you."*)
        - And a `codeBlock` which corresponds to the content of the function. (It tells the parser *"Hey ! Parse a code block as I showed you."*)
    - You can use these syntaxes:
        - `function: [your_declarator] -> name -> parameters -> codeBlock`
        - `function: name -> [your_declarator] -> parameters -> codeBlock`
        - `function: [your_declarator] -> parameters -> name -> codeBlock`
        - `function: name -> [your_declarator] -> parameters -> codeBlock`
    - So here is which kind of syntax you can create:
        - `function toto (...) {...}`
        - `toto function (...) {...}`
        - `function (...) toto {...}`
        - `toto function (...) {...}`

### Order

Be careful, when you will write your configuration file, please respect this order:

- boolean
- variable
- operators
- condition
- parameters
- codeBlock
- if
- function

## Examples

Here you can find some syntax configuration examples:

- JavaScript
    ```
    boolean: ["true", "false"]
    variable{suffix: ";"}: ["let", "const", "var"] -> name
    operators{suffix: ";"}: [
        plus: expression -> "+" -> expression,
        minus: expression -> "-" -> expression,
        multiply: expression -> "*" -> expression,
        divide: expression -> "/" -> expression,
        modulo: expression -> "%" -> expression,
        equal: expression -> ["=="] -> expression,
        assignation: name -> "=" -> expression,
    ],
    condition{prefix: "(", suffix: ")"}: expression
    parameters{prefix: "(", suffix: ")"}: name -> ","
    codeBlock{prefix: "{", suffix: "}"}: many(expression)
    if: ["if"] -> ["else"]
    function: ["function"] -> name -> parameters -> codeBlock
    ```
- Lisp
    ```
    boolean{prefix: "(", suffix: ")"}: ["#t", "#f"]
    variable{prefix: "(", suffix: ")"}: ["define"] -> name
    operators{prefix: "(", suffix: ")"}: [
        plus: "+" -> expression -> expression,
        minus: "-" -> expression -> expression,
        multiply: "*" -> expression -> expression,
        divide: "/" -> expression -> expression,
        modulo: "%" -> expression -> expression,
        equal: "eq?" -> expression -> expression,
        assignation: name -> "=" -> expression,
    ]
    condition{prefix: "(", suffix: ")"}: expression
    parameters{prefix: "(", suffix: ")"}: name -> " "
    codeBlock{prefix: "(", suffix: ")"}: many(expression)
    if{prefix: "(", suffix: ")"}: ["if"] -> [""]
    function{prefix: "(", suffix: ")"}: ["define"] -> name -> parameters -> codeBlock
    ```

## Credits

This multi-language parser exists thanks to Nao and Nathan from Etib !

Here are their contacts:
- Nao
    - [E-mail](mailto:nao.m-lanao@epitech.eu)
    - [LinkedIn](https://www.linkedin.com/in/nao-m-lanao/)
- Nathan
    - [E-mail](mailto:nathan.maillot@epitech.eu)
    - [LinkedIn](https://www.linkedin.com/in/nathan-maillot/)


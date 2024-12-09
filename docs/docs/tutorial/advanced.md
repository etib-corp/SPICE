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

## ABSTRACT SYNTAX TREE (AST)

```yaml
The AST is a crucial intermediary representation that abstracts away unnecessary syntax details, leaving only the essential structure needed for further compilation.
	- Nodes represent code constructs (e.g., loops, conditions, assignments).
	- Used for optimizing and translating code in later stages.
```

## MIDDLEND COMPILER

<!-- See [Configuration](https://www.mkdocs.org/user-guide/configuration/) page on MkDocs site for options. -->

Integration in progress, but the main idea is :

```yaml
The middlend performs optimizations on the AST or Intermediate Representation (IR), ensuring efficient execution.
	- Intermediate Representation: Converts the AST into a lower-level format suitable for optimization.
	- Optimization Techniques:
	- Dead code elimination.
	- Loop unrolling.
	- Constant folding.
```

For example, it ensures efficient memory usage and faster runtime execution.

## BACKEND COMPILER

```yml
The backend takes the optimized IR and generates target-specific machine code.
	- Code Generation:
	- Translates IR into assembly or machine code.
	- Allocates registers and manages memory.
	- Target-specific Optimizations:
	- Applies optimizations based on the architecture (e.g., x86, ARM).
	- Output: Produces an executable file or binary.
```

Example project structure for backend integration:

```yaml
Compiler:
  - Interpreter/
      - Parsing/
      - Semantic Checking/
  - Virtual Machine/
  - Just In Time Compilation/
```


<!-- An example of this is the [Poetry](https://github.com/python-poetry/poetry/tree/master/docs) repo. That  project is also how I got into MkDocs in the first place. -->

<!-- ## Embedding

To embed a gist, just copy and paste the embed script URL which is provided on a gist.

e.g.

```html
<script src="https://gist.github.com/MichaelCurrin/57caae30bd7b0991098e9804a9494c23.js"></script>
``` -->

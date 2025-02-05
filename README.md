# Monkey Programming Language (MPL) interpreter

A result of [this course](https://www.udemy.com/course/develop-an-interpreter-using-rust-programming/learn/lecture/39931676).

Raw notes on the approach / pieces are below.

## Lexer

First stage of the source code processing for an interpreter is lexical analysis.  
It splits code lines into a computer-readable format. E.g. `let x = 1 + 2;` -> `[LET, IDENTIFIER("x"), EQUALS, INT(1), PLUS, INT(2), SEMICOLON]`.

A programming language (e.g. MPL) has:
- keywords (`let`, `fn`, ...) - reserved to denote various language constructions
- identifiers (`x`, `foo`, ...) - user-defined words to name variables / constants
- tokens (`=`, `;`, ...) - reserved characters to form statements

# Parser

Parser makes an Abstract Syntax Tree (AST).  
The Topdown (from the root node) Recursive descend algo (aka Pratt Parser) is used here.  
Statement is a complete program's building block.  
Expressions produces values and could be a part of a statement.  
Statement VS expression highly depends on a programming language.  
Expressions and statements have their own representation in the AST.  
One of our assumption - program is a list of statements.  

Parsing expression is one of the most complex parts of an interpreter due to
- operators priority: `2 + 3 * 4 != 20`
- grouping problem: `(5 + a(10)) * 2` - it's hard to tell `(` for grouping from `(` as a function call

_Me: I disagree with the 2nd point, as fn's `(` will be consumed by the fn statement parser..._  
_Future me: It happened that it's not identifier's parser business to tell variable from a fn call => it's a problem._

Recursive descend algo deals with the challenges.

# Evaluation

This phase makes sense of code => conventions shapes the language. E.g.:
- last expression's result is implicitly returned from the current block
- functions are values that can be assigned and called later or inplace
- ...

There are some hard decisions to make for the phase:
- can non-booleans be interpreted as booleans ?
  > e.g. `vec![] == false`, `"" == false`  
  > Types that can be interpreted as booleans are known as truthy / falsy values.
- what's expression evaluation sequence? end-to-start or start-to-end?
- ... there'are more

There'are evaluation strategies.  
Our choice is **Tree Walking Interpreter**. It evaluates AST nodes on the fly while walking the tree. It's **simple and slow**.  
Slowness could be addressed by preparing a "program" in the form of a byte code or a native code. This program could be executed:
- later (e.g. by JVM) - it makes the *interpreter* closer to a compiler  
- just in time (JIT) - still enough interpreter-ish to me

Evaluation deals with different **values**: integers, string, etc. Evaluation mechanism requires a way to remember the values it produces. Interpreter might use or might not use types of the underlying (host) programming language. We will use rust's types and wrap 'em to make MPL's types.  
The types wrapping has its perf penalty, but we don't care.

Evaluation is stateful in a sense that it should be able to bind values / expressions to identifiers. E.g. `let x = 5; return x;` requires the evaluator to remember `x`'s value. Storing this data is called maintaining an environment. Environment is just a map.  

Functions are simple - they're just objects.
Function calls require stacked environment support in order to (1) be able to define local variables (with values assigned) and (2) be able to access the outer scope variables (unless shadowed).

# Builtin functions and data types

Strings, Vectors and functions like `len(...)`, `sizeof(...)` are useful... and yet missing in the interpreter.  
Adding a type / fn pierces all 3 blocks built: lexer, parser and evaluator.  
**Builtin functions** are pre-defined and provided along with an interpreter. They gets executed by the host language, which is rust. They appear on the evaluation phase.


# The alt module ?

The alt module carries my attempts to make the code code better. Original code has tons of copies of everything, doesn't care if a struct needs to own the value, passes values between helpers carelessly.

The "original" version of the code isn't 1:1 with the course too though.


Gotchas:
- the `Clone` trait is needed to save object's dublicates to the environment while preserving original ones
- the `Display` trait is needed to show program results to the end user. In particular, a function, which contains blocks which may contain expressions which may contain tokens, etc, etc
# ZLisp

## Interpreter structure
1. Parse text stream into lists of tokens
2. Convert the tokens into objects, without having to evaluate them now
3. Basic operations are Lisp operations that are implemented in the interpreter using Zig
    - Composed expressions are built out of the base operations
    - Symbols reside in a LUT
4. Rerun the following loop until we get down to just symbols and primitive procedures
    a. Expressions to be evaluated in environments are reduced to procedures applied to arguments
    b. The procedures in turn are reduced to expressions in new environments
5. Lookup symbol values and primitive procedures are applied directly

## Plans
- [ ] Implement very simple tree traversal interpreter
- [ ] Convert to bytecode interpreter
- [ ] Make a JIT
- [ ] Enable efficient numerical computing based on a the smallest possible set of ops
    - Pretty much like tinygrad, but for this scheme compiler

## Resources
[Tutorial on how to implement a Scheme](http://peter.michaux.ca/)

## Some notes about Scheme
### Quoted expressions
These are represented as a one level nested pair that looks as follows:
```(quote (expr ()))```
The outer pair consists of car(quote symbol) and cdr(expression empty_list pair). The reason for the empty list at the top level list is that the expression can be arbitrarily deep, yet it also needs its own empty list to indicate when the list is terminated. Hence, it really becomes a list in a list, so the top level quote expression needs its own terminator.

## Some notes about Ziig
The beauty of Zig is how it makes memory management very explicit. Sure, it can also be done in C by passing around allocators. But in Zig you essentially have to and the standard library is also setup as such. This results in more well contained logic and generally more potential for performance. The language is also very explicit about what it does and does not do!

## Scratchpad

### Implement chained environments in the form of frames
A frame is essentially a lexical scope defining the top-level variables within that scope. In addition, scopes are chained to the higher level scopes that generated them allowing for "capturing variables" or even "searching in higher level frames" for variables that are not defined in the current scope.

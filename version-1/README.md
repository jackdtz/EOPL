A scheme-like language interpreter in scheme.

The interpreter is constructed using two macros from EOPL: define-datatype and cases

The interpreter internally uses lexical address to represent variable binding, but since it also uses list as its environment for lookup, it is slow.

It supports 

```
math arithmetic
recursion 
top-level definition (define)
if expression 
let expression
closure
function call
variable assignment (set!)
pass by value

```

## How to use it

#### Requirement
1. Racket

```racket interpreter.ss```

Reference: Essentials of programming languages, 2nd Edition


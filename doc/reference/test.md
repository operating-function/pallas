---
id: ref-test
title: Test
---

# Runes

Runes are special symbols or characters that serve as syntactic markers for various language constructs and operations. Runes are a key part of flexible syntax system that is used by Sire, known as Rex (R-expressions), which allows for multiple ways to express the same code structure.

Runes in Sire serve several purposes:

1. **Function Definition and Application**: Runes like `=` for defining functions and `|` for function application are fundamental to Sire's syntax.
2. **Control Flow**: Runes such as `?` for creating anonymous functions or `#datacase` for pattern matching help control the flow of execution.
3. **Metaprogramming**: Some runes, like `^` for expression reordering or `'` for quoting are used in metaprogramming.
4. **Syntactic Sugar**: Runes like `:` for continuation passing style or `-` for infix function application provide convenient shorthand notations.

Runes are one or more non-alphanumeric characters and are used at the beginning of expressions or in specific syntactic positions. They allow Sire to have a highly expressive and flexible syntax while maintaining a simple, uniform structure that's easy to parse and manipulate programmatically.

### Runes

* `|`     function application (can be omitted in nested forms: `(| f a)` is equal to `(f a)`)
* `-`     function application
* `=`     top-level defintion
* `@`     let-binding
* `&`     anonymous lambda
* `?`     named lambda
* `??`  named and pinned lambda
* `,`     tuples
* `:|`   imports
* `####` dependency
* `*`     list of expressions (advanced - this can be used to circumvent the need for indentation)

### Macros

* `[]`    short form for `,`
* `^`      expression reordering ("anonymous where": `(^ f _ b)(expr)` gets rewritten to `(@ _ expr | f _ b))`
* `:`     continuation passing

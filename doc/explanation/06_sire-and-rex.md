---
sidebar_position: 5
id: sire-and-rex
title: Sire and Rex
---

# Sire

Sire is a minimalist functional language that bootstraps itself from PLAN. Unlike PLAN, Sire aims to be realistic for humans to write. In service of this goal, it allows programmers to reference previously defined names in an environment, which it uses [lambda lifting](TK TODO link) to eliminate when compiling itself to PLAN.

For more information on using Sire, see the [tutorial](TK TODO link) and the [language reference](../reference/standard-library/01_bits-booleans.md).

## Rationale

The primary purpose of Sire is to implement more sophisticated languages, which is important because it allows us to achieve a [trustless system](../philosophy/placeholder.md): Sire’s compiler is written in just a few thousand lines of Sire, whose syntax is similar enough to [disassembled PLAN](https://github.com/operating-function/pallas/blob/master/lib/Loot.hs) that the compiler “binary” can be audited by visual comparison to the source code. A more sophisticated language can then simply be implemented directly using Sire source. Nowhere in this chain does the user have to trust an opaque binary. This is in stark contrast to most high level languages which can only be compiled with trusted binaries which are often practically impossible to bootstrap from a high-trust foundation.

While Sire is untyped, it has built-in support for unit tests as a basic code verification measure. The language supports both reflection and reification – it can examine its environment, syntax and terms, and can dynamically construct new code, as well as extend and replace its own syntax through macros. Changing the language in this way gives us a concrete trustless path to an arbitrary number of more advanced languages with better affordances for enforcing correctness and compositionality, using types and other static analysis techniques.

## Rex

Sire uses an unusual syntax called Rex. It’s not necessary to understand Rex to use Sire, except when using macros to change or extend the syntax. Rex is a completely regular and data-oriented abstract syntax that still has a very flexible and expressive concrete syntax thanks to several different input modes. A language implementation that uses Rex would typically transform abstract Rex trees into its own AST, and then compile that to PLAN or interpret it directly, but a Rex tree also contains information on the concrete input mode used in source code.

The point of Rex is to decouple the syntax from the semantics in a way similar to how Lisps tend to use S-expressions, but to also give language developers the ability to offer programmers cleaner and more readable syntaxes than what S-expressions can support. This supports rapid iteration of language designs by removing the need to write new lexers, parsers, pretty printers, and allows optimization code to be shared.

To understand Rex, see the [reference](../reference/standard-library/99_rex.md). To use it, see the [tutorial](../tutorial/intro.md) and [reference](../reference/sire-runes-macros.md#macros) on Sire’s macro system.

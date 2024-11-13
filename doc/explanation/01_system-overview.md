---
sidebar_position: 1
id: system-overview
title: System Overview
slug: '/'
---

# System Overview

Pallas is a stack of technologies. At the bottom lies **PLAN**, an evaluation model. On top of PLAN we have two separate stacks: on one hand **Rex** and **Sire** allow programmers to implement programming languages which compile to PLAN, while a **manifest** format consisting of **cogs** and **drivers** is used to completely specify the behavior of a virtual machine that runs PLAN. We’ll start by explaining how these fit together on a high level, and then go into each one in detail.

At the top, the programmer is offered a programming environment that’s similar to Erlang/OTP, the EVM, and Lisp. Like Erlang/OTP, the programming paradigm is functional, there’s native support for concurrency based on message passing, and programs can be hot reloaded without downtime. Like the EVM, program state is automatically persisted – programs can just keep everything in memory and will run forever (but there is no global consensus). Like Lisp, programs can universally introspect themselves and their environment, and dynamically introduce new code.

At the bottom lies a concurrency-oriented virtual machine whose behavior is completely specified by a user-supplied manifest. The manifest is specified using a standardized format which the virtual machine knows how to interpret and update during operation. This standardized format allows you to move your manifest to a different virtual machine on a different device, and pick up where you left off. The format is guaranteed to be backward compatible, so that programs continue to work even in the future. We aim to also maximize forward compatibility, so that an old machine can come online, receive code updates over the network, and continue to operate with as little degradation of service as possible.

The manifest is encoded in a format called PLAN. PLAN is a lambda-inspired combinator system which is purely functional, lazy, reflective, and has been optimized for data storage. It is very minimal and easy to implement efficiently. PLAN is used as a standard format for arbitrary compute and data, and manifests code for PLAN values of a particular shape, which codes for both compute and managed side effects.

PLAN is human-readable but not human-writeable in practice. To alleviate this, a minimal bootstrapping compiler of a more usable language called Sire is made available, both as Sire source code and as compiled but human-auditable PLAN. Sire is very bare-bones, and is simply intended to be used to implement more capable languages.

Sire uses a syntax called Rex, for R-expressions. Rex is to Pallas as S-expressions are to Lisp: a single highly regular syntax that can be used by many different languages. Rex is almost as simple as pure S-expressions, and simpler than the actual syntaxes of all major Lisps, while being significantly more flexible: Rex can look very much like Python, Haskell or YAML. Using Rex, new languages can be defined as macros on top of Sire, simply rebinding its syntax to new semantics. This allows the system to be flexibly extended while reusing the lexer, parser and pretty-printer.

Taken together, these components give us a resilient, extensible and trust-minimized system that manages persistence, concurrency, portability and real-world interaction in a single unified model. We believe that this will be useful for use cases as diverse as home media servers, AI agent orchestrators, and as an implementation substrate and data syncing layer for p2p protocols.

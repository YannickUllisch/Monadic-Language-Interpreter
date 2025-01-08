# Advanced Programming Course Exam - Haskell Project

This repository contains the code developed for the Advanced Programming Course Exam. The project extends a custom programming language, incorporating key functional programming concepts from the course, and showcases advanced Haskell techniques.

## Overview

The project builds on assignments from the course, implementing features such as:
- A parser for the programming language.
- Language extensions with tuples, `for` and `while` loops.
- Logic operators `OneOf` (||) and `BothOf` (&&), evaluated concurrently.

It demonstrates the use of:
- **Pure Functional Programming** with Monads.
- **Testing with Tasty**, including unit tests and property-based tests.
- **Free Monads** for extensible and modular interpreter implementations.
- **Concurrent Programming** in Haskell.

## Features

### Language Extensions
1. **Tuples**: Add support for defining and handling tuples.
2. **Loops**:
   - `for` loops for iteration with defined bounds.
   - `while` loops for conditional execution.
3. **Concurrent Logic**:
   - `OneOf` (`||`): Returns the result of the first successful concurrent evaluation.
   - `BothOf` (`&&`): Ensures concurrent evaluation of both statements.

### Testing
Comprehensive tests are implemented using the `Tasty` framework to validate:
- Syntax parsing.
- Semantic correctness of new features.
- Behavior under concurrent evaluation.

### Functional Paradigms
- Implementations leverage **Free Monads**, allowing flexibility in adding new interpretations.
- Concepts such as `Reader` and `State` monads for managing environment and state.
- Parallelism achieved through Haskell's lightweight concurrency primitives.

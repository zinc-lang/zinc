<div align="center">

# Zinc lang

The zinc programming language.

</div>

---

## Goals / Features

- Compiled
- Control over allocator(s)
- Scope local context that is implicitly passed to zinc functions
- Support constructs to allow for multiple paradigms, such as:
  - Functional, with currying
  - DOD, ways to specify SOA or AOS
  - OOP, with multiple inheritance
- Powerful compile-time execution
  - Syntax clearly separate for runtime and compile constructs and operations
  - Generics / templates
  - Powerful meta-programming
  - Macros, code generation based on already written code
  - Be transparent about generated code
  - Attributes / annotations, part of the macro system
- Strongly typed
  - Concepts as the preferred means of dynamic dispatch, think c++20x concepts
  - Strong trait/interface system, think rust
  - Concept / trait / interface, all the same thing
  - [runtime polymorphism done right](https://github.com/ldionne/dyno)
- Strong c interop
  - Binding generation from c header source code
- Strong IDE support
- Powerful package and project manager

## Things I want to make with this (to prove its worth)

- Game Engine
  - A commercial game
- OOP UI library, like flutter
  - A file editor
- Programming language(s)
  - Self-hosted zinc compiler
  - POC managed VM language
- POC bare metal OS

## Democracy

I want all semi-major and major decisions to be somewhat democratic.
But democracy is not perfect; I feel if something gets a 51% vote, it's not fair to ignore the other 49%.
Anything below 40% would be a definite and clear no and anything above 80% would be a clear yes.
In the case that it is between 40% and 80%, more consideration needs to take place.
So we can rework the feature to accommodate both sides, etc.

## License

This software is distributed under the terms of both the MIT license and Apache license (Version 2.0) unless any portion is specified otherwise.

See `LICENSE-APACHE`, `LICENSE-MIT`, and `COPYRIGHT` for details.

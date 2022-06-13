# Zinc

For the following two sections, I attempt to give a rough idea of the direction I plan on taking this.
For the items with brackets next to them, that is the main inspiration(s) for it, to give a clearer idea of what I'm talking about.

## Goals / Features

- Compiled
- Control over allocator(s) (odin/zig)
- Multi-paradigm
  - Functional
  - DOD, Array of struct (AOS), Struct of array (SOA)
  - OOP (multiple inheritance)
- Powerful compile-time execution (zig)
  - Good syntax for this too
  - Generics
  - Powerful meta-programming
  - Be transparent about generated code
  - Attributes
- Strongly typed
  - Concepts as the preferred means of dynamic dispatch (c++20)
  - Concepts are effectively the same thing as a trait
  - Strong trait/interface system (rust)
  - [runtime polymorphism done right](https://github.com/ldionne/dyno)
- Strong c interop (zig)
- Strong IDE support

## Things to build (working sub-title)

- Game Engine (unity) (ECS and DOD)
  - A commercial game
- OOP UI library (flutter)
  - A file editor (vscode / intellij)
- Programming language(s)
  - Self-hosted
  - Managed VM language (dart) (c#)
- POC bare metal OS

## Democracy

I want all semi-major and major decisions to be somewhat democratic.
But democracy is not perfect; I feel if something gets a 51% vote, it's not fair to ignore the other 49%.
Anything below 40% would be a definite and clear no and anything above 80% would be a clear yes.
In the case that it is between 40% and 80%, more consideration needs to take place.
So we can rework the feature to accommodate both sides, etc.

## License

This software is distributed under the terms of both the MIT license and Apache license (Version 2.0) unless a portion is specified otherwise.

See `LICENSE-APACHE`, `LICENSE-MIT`, and `COPYRIGHT` for details.

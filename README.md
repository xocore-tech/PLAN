This README exists to provide minimal context for a large, long-running
systems project whose full documentation does not yet exist.

This is the native implementation of the core technologies that came out
of the Plunder project: PLAN, Wisp, Rex, and Pins.

We use literate programming for more of our runtime
system code.  This branch includes the generated code
(e.g. [planvm-amd64.s](./planvm-amd64/planvm-amd64.s)) so that you
can view it on github, and also the generated PDFs, most notably the
[runtime system documentation](./doc/out/planvm-amd64.pdf).  Note that
using the table-of-contents for browsing the code using a real PDF
browser is quite a pleasant workflow.

The high level idea here is to create a self-hosted "purely functional
operating system" which satisfies a novel and challenging set of goals,
all of which have either been achieved in this implementation or have
complete but as-yet unimplemented designs which will satisfy them.

- Orthogonal Persistence (Pins): persist data structures directly and
  transparently to disk to avoid needing to write serialization logic or
  work with an external database.

- Support terabytes of heap data without significant GC pauses. This is
  necessary for orthogonal persistence to be practical, since it
  requires that all application data live on the heap.

- True code-is-data (PLAN): store functions on disk, send functions
  across the network, create new functions, package code into tarballs,
  support builds, deployments, and upgrades fully within the system,
  write functions that disassemble functions, share functions between
  different implementations of PLAN, and run old functions from
  archives.

- Work with any functional programming language within the system: lazy
  or strict, typed or dynamic.

- Have a simple, predictable, and well-specified operational model so
  that the operational behavior of code is mostly consistent across
  implementations.

- Have performance in the same ballpark as GHC, OCaml, or Scheme.

- Zero dependencies: a runtime system written in assembly, with no
  external runtime or toolchain dependencies, eliminating supply-chain
  issues.

- Solve the "bootstrapping problem", where compilers depend on the
  binary of an earlier version of themselves. Instead, implement a small
  programmable macro-assembler (Wisp) for PLAN and use that to build
  everything else.

- Create a new syntax system (Rex) which supports highly expressive code
  (Python, Haskell, Rust) while still being a regular data structure
  which can be arbitrarily transformed (like Lisp S-Expressions).

- Use PLAN, Pins, Wisp, and Rex to create a powerful and convenient
  user-facing languages (Sire and Runa) that are similar to Haskell in
  expressiveness, but which support Erlang-style programming and
  Lisp-style macros.

- Carefully specify and freeze the virtual ISA (PLAN), and the
  bootstrapping language (Wisp) to support full compatibility between
  implementations, and a fully deterministic toolchain.

Almost all of these properties follow from a single, carefully specified
value model and frozen virtual ISA, with the rest of the system layered
on top.

This project was the work of Benjamin Summers (Sol) and Elliot Glaysher
(Iceman), with major design input from Mar (who wishes to remain
pseudonymous). This represents nearly a decade of effort, and there are
many significant innovations here, which will likely be the subject of
substantial documentation. All of the designers have a background in
Urbit, and creating a better realization of those ideals was our core
motivation: something that is fast, powerful, easy to understand, and
systemically simple both in theory and in practice.

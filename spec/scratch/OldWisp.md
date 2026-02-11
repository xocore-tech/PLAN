# Wisp: A Minimal Bootstrapping Language for PLAN

## Overview

**Wisp** is an ultra-minimal Lisp-shaped language designed to bootstrap
**PLAN**, a compact, lazy functional runtime and representation system.

Wisp is not intended to be a pleasant or stable user-facing language.
Its purpose is to be:

- small enough to audit and reimplement,
- expressive enough to define its own higher-level replacements,
- powerful enough to bootstrap an entire language stack,
- independent of external binaries or opaque tooling.

In the intended architecture:

``` text
PLAN  (runtime / calculus / persistence substrate)
  ↑
Wisp  (minimal reader, macro system, evaluator)
  ↑
Sire  (typed, ergonomic language defined in Wisp)
  ↑
Runa  (full Haskell-like language)
```

Wisp exists to **construct and transform PLAN values**, not to define
semantics beyond what PLAN already provides.

------------------------------------------------------------------------

## Core Insight

The most important design principle of Wisp is this:

> **There are no fundamental syntactic forms.**
>
> Everything---including `law`, `def`, `mac`, `eval`, and `pin`---can be
> defined as macros.

Some of these are provided as conveniences in the reference
implementation, but *none are required*, and none are privileged by
evaluation semantics.

This makes Wisp a *language bootstrap*, not a fixed language definition.

------------------------------------------------------------------------

## PLAN Relationship

PLAN is the actual computational substrate. It provides:

- application (`A`)
- laws (`L`)
- numeric atoms (`N`)
- pinned values (`P`)
- lazy evaluation

Wisp does **not** add new runtime semantics. Instead, it:

1.  Reads syntax
2.  Encodes it into PLAN values
3.  Rewrites those values using macros
4.  Evaluates the resulting PLAN program

Wisp's evaluator evaluates **PLAN values**, not Wisp syntax.

------------------------------------------------------------------------

## Surface Syntax

### Lexical Rules

- Whitespace separates tokens
- `;` begins a comment until end of line
- Strings are `"..."` (no escapes)
- Symbols are arbitrary non-delimiter character sequences
- Digit-only tokens are naturals

### Parsed Forms

Wisp parses input into a minimal surface AST:

- `Sym name => name`
- `Nat n => (1 n)`
- `Str s => (1 s)`
- `( ... ) => (0 ...)`

TODO: probably the right way to define this is with lexicographical /
parser definitions, like in a normal language specification?  We shouldn't
make reference to the `List` and Vect forms, etc because those will
probably be removed to reduce code size further.

### Syntactic Sugar

A symbol immediately followed by a grouped form parses as application:

``` wisp
f(..) ⇒ (f (..))
f[..] ⇒ (f [..])
f{..} ⇒ (f {..})
[..]  ⇒ (BRAK (..))
{..}  ⇒ (CURL (..))
```

This is reader sugar only.  For example, this makes it possible to define
forms like the following, just by defining macros:

```
`(quasi ,(quoted) ,@(form))
~[list literal]
#[array literal]
{f x => x}
```
------------------------------------------------------------------------

## Encoding into PLAN

All surface forms are encoded into PLAN `Val` values.

- Symbols and strings are encoded as naturals (base-256)
- Lists are encoded as application chains rooted at a distinguished head
- No separate AST survives past this phase

Macros and evaluation operate on these PLAN encodings.

------------------------------------------------------------------------

## Two Distinct Macroexpansion Regimes

This is a critical distinction.

### 1. Evaluation-Time Macroexpansion (Wisp Macroexpander)

This is the macroexpansion most users think of.

It occurs:

- before evaluation
- recursively
- on PLAN list encodings

A form is eligible when:

- it is a list,
- its head is a symbol,
- that symbol is bound as a macro,
- and the symbol is not shadowed by compilation-local rules.

Macros are PLAN functions that receive the current environment and the
original form, and return:

- an updated environment
- a replacement PLAN value

Expansion continues until no macro applies.

This mechanism alone is sufficient to define *all surface syntax*.

------------------------------------------------------------------------

### 2. Compilation-Time Expansion Inside `law`

`law` does **not** participate in evaluation-time macroexpansion.

Instead, `law` is a *constructor macro* that performs its own,
independent expansion pass while compiling a function body into a PLAN
`L` value.

Inside `law`:

- a flat table of local names is established (self, args, binds)
- references to those names are rewritten into numeric indices
- macroexpansion must be done manually, under a different discipline:
  - local names must *not* be treated as macros
  - global macros may or may not be allowed, depending on policy
- globals are either embedded as constants or rejected

This expansion is part of **law's implementation**, not part of Wisp's
general macroexpander.

If `law` were implemented as a user-level macro, it would need to
reimplement this logic explicitly.

------------------------------------------------------------------------

## Evaluation (Orthogonal to `law`)

After evaluation-time macroexpansion completes, Wisp evaluates the
resulting PLAN value.

Evaluation rules are minimal:

- Atoms resolve through the global environment
- Lists evaluate element-wise, then apply left-associatively
- Evaluation is lazy due to PLAN's representation and host semantics

Importantly:

- **There is no evaluation rule for `law`.**
- A `law` form should never reach evaluation.
- Only the PLAN `L` value produced by `law` is evaluated.

------------------------------------------------------------------------

## Built-in Conveniences (Not Fundamental)

The reference implementation includes several built-in names. None are
required.

### `def`

``` wisp
(def name expr)
```

Evaluates `expr` and binds the resulting PLAN value globally.

### `mac`

``` wisp
(mac name expr)
```

Evaluates `expr` and binds it globally as a macro.

### `eval`

``` wisp
(eval expr)
```

Evaluates `expr` during macroexpansion and splices the result.

### `pin`

``` wisp
(pin expr)
```

Evaluates `expr` and constructs `P expr`.

`P` is **not** a quote or evaluation blocker. It is a structural hint to
PLAN used to influence:

- memory layout
- disk layout
- network layout

In an orthogonally persistent system, `P` affects representation, not
semantics.

### `law`

``` wisp
(law (name arg1 arg2 ...)
  (@ local expr)
  ...
  body)
```

A shorthand for constructing a PLAN `L` value.

`law`:

- establishes a flat local namespace
- performs its own compilation-time macroexpansion
- rewrites names to numeric indices
- emits a canonical PLAN lambda representation

It is **purely a constructor** and can be replaced entirely.

------------------------------------------------------------------------

## Scoping Model (Compilation Only)

Scoping rules apply **only during `law` compilation**.

Inside a `law`:

1.  `self` → index 0
2.  arguments → indices 1..n
3.  local binds → subsequent indices
4.  globals → embedded or rejected

There are:

- no nested lexical scopes
- no closure capture
- no environment chains

This simplification is deliberate and dramatically reduces runtime and
compiler complexity.

------------------------------------------------------------------------

## Replaceability and Evolution

Because:

- macros are ordinary PLAN functions,
- macroexpansion is explicit and programmable,
- no syntax is privileged,

it is straightforward to:

- replace `law` with a richer lambda macro,
- add nested scoping at a higher level,
- introduce pattern matching, types, effects, etc.,
- or discard all built-ins and rebuild them differently.

Wisp is intentionally *not* the final language---it is the seed.

------------------------------------------------------------------------

## Summary Mental Model

- Wisp reads text into PLAN values
- Macros rewrite PLAN values
- Evaluation runs PLAN programs
- `law` builds PLAN lambdas but is not part of evaluation
- All "syntax" is optional and replaceable
- PLAN is the only true semantic foundation

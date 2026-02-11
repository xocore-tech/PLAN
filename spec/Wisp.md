# Wisp Specification

TODO: Document the fact that every top-level form is wrapped with EVAL
and that this makes it possible to overwrite and extend all Wisp behavior
except the reader and the state machine model.

TODO: And also, the above means that the actual core wisp evaluation
flow only does macro-expansion: it is the EVAL macro which actually
drives evaluation.

## Overview

Wisp is a simple lisp-shaped language for constructing and evaluating
complex PLAN values.  You can think of Wisp as a macro-assembler for
PLAN which happens to look a lot like a Lisp.

The production version of Wisp and PLAN is implemented raw assembly
language, without linking against the C standard library.  This is
a part of a larger effort to produce a system which has *zero binary
dependencies*.

Because of this demanding requirement, Wisp is extremely small, has
extremely few features, and can be quite unforgiving.

However, Wisp still needs to be expressive enough to serve as a foundation
upon which a much more featureful and friendly language ecosystem can
be constructed.

In order to achive this, Wisp works against a single input text, and
works by threading everything through a single global environment,
which can be arbitrarily modified by macros.

Wisp include an "include" syntax to let Wisp files depend on other
wisp files, but the semantics are defined as if all of these files were
concatenated together into a single input stream which is fed into a REPL.


## Wisp Syntax

Wisp expressions are an extremely simplified version of s-expressions,
and encoded as PLAN values.

Wisp is an extremely bare-bones system designed for bootstrapping from
a tiny kernel, so it is extremely permissive in any case where that
simplifies implementation.

Wisp input works on ASCII bytes, and, in addition to the normal symbol
characters, treats all bytes above 127 as symbol characters.  As a
result, UTF-8 identifiers are supported transparently without requiring
UTF-8 decoding.

### Intuition

Wisp notation is intended to provide the smallest practical textual
interface to PLAN values. The grammar defines only grouping, sequencing,
and atomic tokens, and deliberately avoids encoding any semantic
distinctions such as function application, binding, or control flow. As
a result, all composite forms have the same syntactic status, and all
interpretation is deferred to later macroexpansion and evaluation stages.

In practice, Wisp source resembles a minimal Lisp: parenthesized forms
are used by convention to represent application and macro invocation,
while bracketed and braced forms provide additional structural markers
for higher-level syntactic layers. However, these conventions are not
enforced by the syntax itself and may be freely reinterpreted or replaced
during bootstrapping.

Wisp lacks the usual Lisp reader notation for quotation, quasiquotation,
arrays, and related constructs. Instead, a small amount of light syntactic
sugar is provided, enabling slightly less concise but equivalent
notations. These notations are not built in; they can be implemented
entirely using Wisp’s simple macro system.

    `(quasi ,(quoted) ,@(form))
    ~[list literal]
    #[array literal]
    {f x => x}

### Forms

Here are the core lexicographical and syntactic forms.  Note that this is
LL(1) and can be parsed using a simple recursive descent parser without
backtracking.

    comment: /;[^\n]*\n/
    string:  /"[^"]*"/
    symbol:  /[^ \n;()[\]{}"]+/
    gap:     (space | newline | comment)+
    inner:   gap? expr (gap expr)* gap?
    para:    '(' inner? ')'
    brak:    '[' inner? ']'
    curl:    '{' inner? '}'
    nest:    (para | brak | curl)
    juxt:    symbol nest
    expr:    (juxt | symbol | string | nest)
    file:    gap? (expr gap?)* EOF

### Syntactic Sugar

These rewrites are purely syntactic and are applied prior to PLAN encoding.

    symbol(..) => (symbol (..))
    symbol[..] => (symbol [..])
    symbol{..} => (symbol {..})
    [..]       => (BRAK (..))
    {..}       => (CURL (..))

We'll use this notation to describe the encoding of forms as PLAN values.

    234                  -- natural number
    encode("a")          -- byte string as a natural via LSB-first encoding
    read("1234") -> 1234 -- numbers interpreted from decimal string
    (a b)                -- application
    (a b c) => ((a b) c) -- shorthand

### Basic Forms

The following rules define a total, deterministic mapping from parsed
forms to PLAN values.

    symbol   => (1 read(symbol))   -- if symbol matches /[0-9]+/
    symbol   => encode(symbol)     -- otherwise
    "string" => (1 encode(string)) -- string contents, no escaping
    ()       => 0
    (form..) => (0 form..)

## Wisp Evaluation Model

This section specifies the evaluation process for Wisp forms. It describes
how an input form is transformed and executed given an environment,
and how macroexpansion participates in that process.

### Overview

Wisp evaluation is defined as a state machine over an explicit environment
value, itself represented as a PLAN value.

    wisp : (Plan, Plan) -> (Plan, Plan)
    wisp (environment, input) -> (environment, output)

Both macroexpansion and evaluation operate within this state machine and
may update the environment as part of their operation.

The environment is represented as a PLAN value encoding an unbalanced
binary search tree of global bindings. Each binding associates a symbol
with a value and an optional macro tag:

    bst = 0 | (0 key:Nat val:Plan isMacro:Bool left:Bst right:Bst)

Wisp evaluation proceeds in two distinct stages:

- **Macroexpansion**, which rewrites forms structurally.

- **Evaluation**, which translates the resulting wisp expression into
  a PLAN expression and evaluates it.

Macroexpansion determines the *shape* of a program, while evaluation
determines the *behavior* of that shape.

### Special Forms

There are five built-in special forms that participate in macroexpansion
as if they were macros, but do not exist as bindings in the environment.

These special forms are: `EVAL`, `def`, `mac`, `pin`, and `law`. Their
behavior is specified in detail in a later section.

Although it is possible in principle to implement these forms as
ordinary macros, doing so would complicate bootstrapping. Such an
implementation would require materializing their behavior as concrete
PLAN values, which in turn would require storing opaque binary artifacts
outside the system.

To keep bootstrapping simple, linear, and free of external artifacts,
the initial implementations of these forms are provided directly by the
runtime system rather than as PLAN values in the environment.

Conceptually, each built-in special form could be implemented as a
user-defined macro given sufficient bootstrap support. Their special
status is representational rather than semantic. As a result, their
behavior may be extended, refined, or replaced entirely by redefining
them as macros within Wisp programs.

### Macro Expansion Phase

Macroexpansion is responsible for introducing binding structure,
defining control constructs, desugaring surface syntax, and generally
shaping a program into a form suitable for evaluation.

A form is eligible for macroexpansion when it is a composite form whose
first element is a symbol, and that symbol is recognized as a macro.

A symbol is considered to be defined as a macro if either:

-   The symbol is bound in the environment and tagged as a macro.

-   The symbol is unbound in the environment, but is one of the five
    built-in special forms.

Once a macro form has been identified, the macro logic is invoked with
the current environment and the entire form.  The macro returns a new
environment and a replacement form. Macroexpansion is then applied
recursively to the resulting form. Macroexpansion therefore computes a
fixpoint under macro rewriting.

If a form is not composite, it is returned unchanged. If a form is
composite but not eligible for macroexpansion, macroexpansion is applied
recursively to each subform from left to right.

Macroexpansion is explicit, programmable, and stateful. Macros may
compute, inspect, construct, and replace arbitrary forms while updating
the environment.

### Embedded Values

Wisp provides a uniform mechanism for embedding constant PLAN values
directly into forms.

The encoding of natural numbers as Wisp expressions uses the form `(1 n)`,
but this `(1 value)` representation may be used for any constant PLAN
value. As a result, macros need not reify PLAN values as syntactic
expressions in order to embed them into Wisp code.

This design eliminates the need for quotation at the Wisp level, as PLAN
values may be embedded directly into syntax during macroexpansion.

### Evaluation Phase

Given a fully macroexpanded form, evaluation proceeds by translating
the form into a corresponding PLAN expression, and then evaluating it.

Evaluation follows these rules:

- Constant forms are used verbatim.

- Symbols are resolved by lookup in the environment.

- Composite forms are translated recursively to PLAN application trees.

This translation is nearly one-to-one, differing only in symbol resolution
and application re-association.  All evaluation logic is handled by
PLAN itself.

### Example (Informal)

The following example illustrates the relationship between Wisp forms,
PLAN expressions, and PLAN evaluation.

Given the Wisp form:

    (x 3 (x 4 4))

with an environment binding `x` to a PLAN value representing addition,
macroexpansion produces no change. Evaluation translates the form into
the PLAN expression:

    (Add 3 (Add 4 4))

PLAN evaluation then proceeds according to its own rules, ultimately
producing the result:

    11

This example is illustrative only; it does not introduce additional
evaluation rules beyond those described above.

### Summary

- Wisp evaluation is a state machine over an explicit environment.
- Macroexpansion rewrites program structure and may update the environment.
- Special forms participate in macroexpansion without existing as values.
- Evaluation translates macroexpanded forms into PLAN expressions.
- All computation semantics are provided by PLAN.
- No syntactic construct has intrinsic evaluation semantics.

This separation allows Wisp to remain minimal, extensible, and suitable
for bootstrapping richer language layers.

## Special Form Semantics

This section specifies the behavior of the five built-in special forms:
`EVAL`, `def`, `mac`, `pin`, and `law`.

Each special form participates in macroexpansion as if it were a macro,
but is implemented directly by the runtime system rather than as a PLAN
value in the environment. Their special status is representational rather
than semantic; equivalent behavior may be achieved by user-defined
macros given sufficient bootstrap support.

### `EVAL`

**Form:**

    (EVAL expr)

**Behavior:**

During macroexpansion, `EVAL` evaluates `expr` as a Wisp form in the
current environment. The resulting PLAN value is embedded directly into
the surrounding program as a constant.

Operationally:

1. `expr` is evaluated using the current global environment.
2. The resulting PLAN value `v` is returned as `(1 v)`.

**Notes:**

- `EVAL` allows computation to occur during macroexpansion.
- The evaluation performed by `EVAL` follows the same rules as normal
  Wisp evaluation.
- Because the result is embedded as a constant, it is not re-evaluated
  at runtime.


### `def` and `mac`

**Form:**

    (def name expr)
    (mac name expr)

where `name` is a symbol.

**Behavior:**

During macroexpansion, `def` evaluates `expr` and binds the resulting
PLAN value to `name` in the environment.

Operationally:

1. `expr` is evaluated in the current environment.
2. The environment is updated to bind `name` to the resulting value,
   without the macro tag.
3. The form evaluates to bound value.
4. If the `def` form is used, the macro flag is set to 0.  If the `mac`
   form is used, it is instead set to 1.

**Notes:**

- Bindings introduced by `def` are visible to subsequent macroexpansion
  and evaluation.
- `def` introduces global bindings only.


### `pin`

**Form:**

    (pin expr)

**Behavior:**

During macroexpansion, `pin` evaluates `expr` and constructs a pinned
PLAN value from the result.

Operationally:

1. `expr` is evaluated in the current environment.
2. The resulting PLAN value `v` is wrapped using the PLAN `P` constructor.
3. The pinned value is embedded as `(1 (P v))`.

### `law`

**Form:**

    (law signature binding* body)


The `law` special form constructs a PLAN `L` (law) value. It operates
entirely during macroexpansion and produces a constant PLAN value. It
introduces no runtime evaluation rules.

The syntactic structure of `(law ...)` mirrors the structure of the PLAN
`L` constructor. The primary role of `law` is to translate symbolic Wisp
syntax into the corresponding indexed PLAN form.

### Expansion Process

The law forms works by performing the following steps:

1. Parse the law-expression, the signature, and the table of bindings.

2. Collect a table of local bindings.

3. Perform an modified macroexpansion pass on each binding expression
   and also on the body.

4. Translate from Wisp expressions into law-body expressions, resolving
   names into local indexes or global constants.

5. Construct the final PLAN Law.


#### Parsing `(law ...)` forms

The `signature` form is a composite form of at least two symbols:

    (self arg₁ ... argₙ)

Each `binding` has the form:

    (@ name expr)

#### Review of PLAN `L` Structure

A PLAN law value has the form:

    L arity self binds

where:

- `arity` is the number of positional parameters,
- `self` is a natural number identifying the function itself,
- `binds` encodes local bindings and the body.

Bindings are are represented as a list of PLAN nodes, each of the form:

    (1 expr next)

The final expression represents the body expression.

Note that this table of bindings is recursive, each supercombinator is
associated with a single top-level LETREC.

Binding and body expressions take on the following forms:

    (0 v)          -- the constant value v
    (0 f x)        -- A function application of the expressions f and x
    0              -- self reference
    1              -- the first argument
    arity          -- the last argument
    arity+1        -- the first LETREC binding
    arity+nbinds-1 -- the last LETREC binding
    other          -- Anything else is treated as an unquoted constant value.

The primary job of the `law` form is to resolve names into indexes and
to translate syntax into the above PLAN shape.

#### Local Namespace

Once The binding table has been parsed, we can determine the full set
of bindings available in the local scope (self + arguments + binding).
We will use this table to assign a number to each binding and to determine
if a binding is bound locally.

`law` constructs a flat mapping from symbols to numeric indices:

1. `self` → index `0`
2. positional parameters → indices `1 .. n`
3. binding names → subsequent indices, in order

This namespace exists only during `law` processing.

#### Restricted Macroexpansion and Compilation

`law` performs its own macroexpansion and compilation pass over
binding expressions and the body.  This works mostly like the normal
macroexpansion pass except that local bindings can shaddow global macro
bindings.

#### Final Construction

Let:

- `k` be the number of positional parameters,
- `self` be the self identifier,
- `bindings` be the compiled binding expressions,
- `body` be the compiled body expression.

`law` constructs:

    L k (N encode(self)) continuation

where `continuation` is formed by folding bindings over the body.

The resulting `L` value is embedded as a constant.

## Summary

Wisp is a deliberately minimal language layer that provides a textual
interface to PLAN while imposing as little structure as possible.
Its syntax defines only grouping and atomic values, its evaluation model
introduces no intrinsic semantics, and all computation is delegated to
PLAN.

Meaning in Wisp arises through macroexpansion. Programs are shaped by
structural rewriting and explicit environment manipulation rather than
by built-in syntactic constructs. Even fundamental features such as
binding and function construction are introduced through macro-like
mechanisms, allowing the language to evolve without modifying the
runtime.

A small set of built-in special forms is provided solely to simplify
bootstrapping. These forms behave like macros but are not represented as
PLAN values, preserving an empty initial environment and avoiding opaque
artifacts. Their behavior is representational rather than semantic and
may be replaced or extended by user-defined abstractions.

This separation of syntax, macroexpansion, and evaluation allows Wisp to
serve as a stable foundation for layering richer languages. Higher-level
systems can be constructed incrementally, using Wisp as a transparent
and extensible bridge between human-readable notation and PLAN’s
execution model.

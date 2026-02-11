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

These special forms are: `eval`, `def`, `mac`, `pin`, and `law`. Their
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

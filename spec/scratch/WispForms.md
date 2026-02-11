## Special Form Semantics

This section specifies the behavior of the five built-in special forms:
`eval`, `def`, `mac`, `pin`, and `law`.

Each special form participates in macroexpansion as if it were a macro,
but is implemented directly by the runtime system rather than as a PLAN
value in the environment. Their special status is representational rather
than semantic; equivalent behavior may be achieved by user-defined
macros given sufficient bootstrap support.

### `eval`

**Form:**

    (eval expr)

**Behavior:**

During macroexpansion, `eval` evaluates `expr` as a Wisp form in the
current environment. The resulting PLAN value is embedded directly into
the surrounding program as a constant.

Operationally:

1. `expr` is evaluated using the current global environment.
2. The resulting PLAN value `v` is returned as `(1 v)`.

**Notes:**

- `eval` allows computation to occur during macroexpansion.
- The evaluation performed by `eval` follows the same rules as normal
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

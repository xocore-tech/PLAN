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

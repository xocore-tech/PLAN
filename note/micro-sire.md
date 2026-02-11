# μSire

Especially now that we are using XPLAN, I wonder if it would be possible
to build an even smaller bootstrapping seed using an ultra-minimalist
lisp-style language which is used to implement both itself and Sire.


## An Even-More Minimal Sire

Sire is designed to be scalable up from something tiny into something
fairly expressive all within one system, but the result is a little bit
complicated.  Sire has module system, a property system (used for types,
pattern matching, etc), and a highly expressive syntactic system.

A lot of complexity could be removed by using s-expressions instead of
Rex, an effectful reader instead of a functional parser, a single
namespace instead of a module system, and no property system.  The reader
could just read a single byte at a time, removing the need for any sort
of buffering.

Similarly, using an s-expression-based syntax will significantly reduce
the footprint of the printer used during bootstrapping.


## Primops replace Jets

Instead of needing to include all of the code for jets, we can simply add
an XPLAN primitive for calling into a jet by code.

    = (**Inc x)   | ##4 | 0 x
    = (**Dec x)   | ##4 | 1 x
    = (**Add x y) | ##4 | 2 x y

This will *significantly* reduce the seed size, and it will also make
it easier to iterate on things like Blitz by building the implementation
first and getting the formal implementation right only later.

Furthermore, this will also make it possible to eliminate jet matching
from the runtime system altogether.  During bootstrap we use these
primops, and then later, for deterministic code, we just use an online
compiler to match jets to their implementations.


## Bootstrapping with μSire

To bootstrap with μSire, you would use usire.seed to load and run the
full Sire implementation.

    ./plan usire.seed usire/sire.lisp args..

The net result is a much smaller bootstrap.  A smaller seed, and a
smaller runtime system.


## The Blitz Primop

The primop that is used to access Blitz behavior would do no validation
and would just execute the supplied bytecode as-is.

This would just use a dumb Blitz interpreter.  Once there is an online
compiler, this would instead work by having the compiler validate the
blitz code and then codegen directly to asm, avoiding this machinery
altogether.

The only purpose of this machinery is to make it possible to do things
that have some performance demands in contexts that run before the online
compiler is in place.


## How much smaller?

I estimate that over half of the functionality could be removed from
the bootstrapping seed using this approach.


## μSire Language Proposal

S-expressions would have symbols, nats, and arrays.

    '3     => [3]
    'a     => %a
    '"a"   => [%a]
    '3a    => "3a"
    '#     => "#"
    '(f x) => [%f %x]
    '(f 3) => [%f [3]]
    '(f)   => [%f]      ;; Note that this is the same as "f"

These primitive forms:

    (define n expr)
    (function (f args..) body)
    (inlined (f args..) body)
    (let n x b)
    (letrec ((n x)..) b)
    (unittest x y)
    (inline f args..)
    (block)
    (block x)
    (block x xs..)

And this syntactic sugar:

    (lambda (args..) body)
        => (function (0 args..) body)

    (define (f args..) body)
        => (define f (function (f args..) body))

    (lets () b)
        => b

    (lets ((n x) binds..) b)
        => (let n x (lets binds..) b)

    (function (f args..) (define a x) a)
        => (function (f args..) (let a (x y) body))

    (& args.. body)
        => (function (0 args..) body)

    (? (f args..) body)
        => (function (0 args..) body)

    (= n x)
        => (define n x)

    (tests (a b))       => (unit-test a b)
    (tests (a b) abs..) => (block (unit-test a b) (tests abs..))

These bindings would already be in place.

    #0 #1 #2 (pre-bound symbols meaning <0>, <1>, <2>)

And these special syntactic forms would eliminate all of the line-noise.

    [a b c]      ==>  (function (0 a b) c)
    [a b => c]   ==>  (function (0 a b) c)
    {f a b c}    ==>  (function (f a b) c)
    {f a b => c} ==>  (function (f a b) c)

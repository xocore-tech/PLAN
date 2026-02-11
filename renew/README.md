This is a small utility to convert from the old seed format to the
new one.  This makes it possible to use the old Haskell runtime to
generate test inputs for the new runtime.

## What Changed

In the old format, every node had a tag to indicate if it was an atom
or a cell.  For example:

    $(((0 1) (2 2)) 3)

Would be:

    0001 $0 1 $1 01 $2 1 $2 1 $3

So, you end up with essentially a unary encoding of the closure size
with a terminator. "0b0001", etc.

This wastes a lot of space with big closures, and it's complicated
to decode.

So, the format was changed to store the size-of-the-size in unary,
followed by the size in binary (minus the high bit, which is always 1).

For example:

    $(((0 1) (2 2)) 3)

Then becomes

    0011 $0 1 $1 01 $2 1 $2 1 $3

The result is the same size for this example, but 

(TODO: This might be wrong, I wrote this off the top of my head after
poking around briefly to try to remember how this works).


## Usage

```bash
make -C renew
renew/renew foo.oldseed foo.newseed
```

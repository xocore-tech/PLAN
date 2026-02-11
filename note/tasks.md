This is some notes about the relationship between actors and the
"sub-interpreter" concept which is used for exception handling.

Basically, you would have a "Task" concept which represents a paused
computation.  Each task would be a stack and a table of bindings which
can be addressed by the outside task using handles.

Each task would be a data structure in the runtime system, and each would
have a pointer to the process that it belongs to.  Each process would
have one main task (and several sub-tasks), and would have a scheduler,
a mailbox, a heap, etc.

Tasks are represented as values in Blitz, but linear types are used to
enforce explicit resource managements.  Also, tasks are a separate type
and cannot be used as PLAN values, cannot be sent to another process, etc.

In the actual runtime system, sub-tasks should be represented as a
number which is a key into a table.  This table would likely live on
the actor itself.


## Ideas for Faster Exceptions

One of the downsides here is that allocating a whole task for each
exception-handling context is pretty expensive.  It might be possible
to have some specialized paths that avoid allocating a sub-task until
on affect actually needs to run?

-   If the seal concept takes a maximum effect value (and everything
    above that is an exception), then maybe the initial execution can
    run using the same stack?

-   Run a subcomputation where all effects are handled in the same way
    except errors, which propagate upwards?

On error:

    [a b c f x seal a b c d exn throw]
          /|\                        | 
           |                         | 
           \______exnCode[exn]_______/

On effect:

    Allocate a new task.  `task =`

        task = [0=arg][trampoline a b c d arg effect]

    [a b c f x seal a b c d arg effect]
          /|\                    | 
           |                     | 
           \_(task, ef, thunk=0)_/

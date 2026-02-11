# Productionization Roadmap

## Escaping The Haskell Dependency

The first thing to note here is that there are some changes to PLAN itself
which have not been incorporated into the runtime system, and which cannot
be completed until the Haskell runtime system is not longer a dependency.

Right now, we need the Haskell runtime for a few things:

- Generating Seeds
- Reasonable UX for running Sire
- Reasonble Execution Speed for development.
- Running Cog/Drone Demos

So, the first hurdle is to eliminate this dependency.  My suggestion
path to that is as follow:

1.  Redesign Wisp to have built-in special forms instead of having all
    forms be defined as macros.

2.  Directly implement Wisp in assembly.

3.  Rewrite all seed-based testing to use Wisp instead.

4.  Eliminate from the native runtime altogether.  At this point, there
    is no longer any dependency on the Haskell runtime for generating seeds.

5.  Implement Rex (ordrex) in Wisp.

6.  Implement Sire in Wisp.

7.  Port the pretty-printer to Wisp, and achive decent debugging and
    error message printouts in Sire-in-Wisp.

8.  Port the Haskell Sire conveniences to Wisp (file loading, build
    caching, colored printing).  At this point, we no longer need Haskell
    for Sire execution, but it is still going to be pretty slow.

10. Finish the PlanGrm optimizer, producing assembly.

11. Implement the lifecycle stuff for an optimizing compiler (allocation
    hooks, etc).

12. Implement the optimizing compiler for serious.  At this point, the
    native runtime should be significantly faster than the Haskell runtime,
    eliminating the need to rely on the Haskell runtime for development
    cycle speeds.

13. Add basic actor model primitives to the runtime system.

14. Reimplement cog/drone in Sire/XPLAN using this process model

15. Get cog/drone demos working using this new system.


## Integrate PLAN Changes

At this point we no longer need the Haskell runtime system at all,
which opens XPLAN up to evolution again.

1.  Change the MkLaw ABI to match the spec. (<1> (0 a m b)) or whatever.

2.  Make pins and laws lazy by default, track normalization state,
    normalize them before moving to GC2.  Make sure Judge is lazy-safe.
    Making pins and laws lazy should make iterative construction of pins
    and laws much faster (hitch, etc)

3.  Rework XPLAN to be closer to something potentially freezable.
    Right now it is an unorganized dumping ground or random effects.


## Deterministic Execution

At this point, cog/drones will be running XPLAN, not PLAN, so
replayability and safety will depend on trusting user code to obey rules
(no effects, no updates to unowned data).

1.  Figure out memoziation and assign cryptographic identities to pins.

2.  Add jet matching to the online compiler (needed to make PLAN fast).

3.  Standardize 

4.  Figure out the design and implementation for sandboxing.

    We should have an execution mode which disables effects, and
    another mode which enables them, but to what extent should we allow
    sandboxed code to talk to outside code via effects, and how can that
    be implemented most efficiently.

5.  Figure out the design of Blitz and freeze it.

    The design I have now basically works and is plausibly simple enough,
    but the supporting tooling is complicated.  Once the design of the
    online compiler stabilizes, it is worth evaluating that design from
    the perspective of that tooling, since we ideally want something
    that integrates easily and seamlessly into that optimization path.

6.  Rewrite all imperative array algorithms using Blitz instead of XPLAN.


## Xenophobia

At this point, we should be able to run Cog/Drone software properly,
in a constrained execution environment with deterministic execution.

The next step is to be able to run fulltag (or other programs) in a fully
self-hosted environment, with our programs being written entirely in
text with no binary dependencies, and no need for any programs written
in alien languages.

1.  Implement another runtime system in Javascript: JPLAN.  Like XPLAN,
    this should also be a superset of PLAN, but support making arbitrary
    javascript calls.  This will be a bit tricky, since Javascript does
    not have TCO support.

2.  Implement an efficient serialization system: Silo (byte-oriented, not
    bit-oriented).

3.  Implement some libraries for interacting with the dom.

4.  Load the JPLAN runtime from the browser in fulltag, and then have
    it load the UI logic as a JPLAN value.

5.  Port our fulltag frontend over to JPLAN, using the same Wisp-booted
    build for both the front-end and back-end code.

6.  Rewrite the supporting bash scripts for driving fulltag into XSire
    scripts.

At this point, we should be able to run the entire system as a xenophobic
webapp.

    gcc plan.s -o plan               # assemble the runtime
    ./plan fulltag $(which chromium) # loads wisp/fulltag.wisp


## Tooling and Productionization

At this point, we will have a fulling working xenophobic stack, but
we will need to build a bunch of tooling before we can really use it
in anger.

1.  We will need a way to do live updates, and debugging tools for cog
    state, the ability to handle errors, and rollback to earlier states.

2.  We will need to build Runa, and get a nicer language going, Sire is
    not going to cut it at this stage.

3.  We are going to need to implement a bunch of reflection and
    introspection tooling in XPLAN for inspecting the state of the
    runtime system.

4.  We are going to need to build out a proper BEAMesque actor model
    system with a decent scheduler.

5.  We are going to need to port over pretty big swaths of code from
    Haskell and Erlang, and probably find some sort of LLM-enabled way
    to automate that.

6.  We will need to stabilize Blitz (and it's integration with Runa),
    PLAN, and XPLAN.  We will need to standardize a way to encode mutual
    recursion and integrate that into the online compiler.


## Becoming an Operating System

At this point we will have a real system which we can begin to use
for basic production use cases, but we will still be doing our actualy
development work in Unix.

The next step is to begin dogfooting actual tools.

1.  Some type of PLAN-native CLI.

2.  Version Control

3.  Internet Protocols

4.  Text Editing

5.  Continueous Integration

6.  Deployment Tooling

At is at this point when we can actually start reaping real wins from
the architecture, instead of just being handicapped by it.


## Unikernel, Arm, etc

Once this is productionized and put into practice, it will eventually
make sense to abandom the amd64/linux dependency and build an actual
unikernel which runs directly against the hardware.  The persistence
and scheduling subsystems in particular can benefit enormeously from
escaping the syscall interface.

However, at this point, we will definitly need to employ the services
of systems specialists, and we probably need insitutional buy-in to
make any real progress here.

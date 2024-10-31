A.  A jet should be a useful primop within a plunder runtime.

    1)  It should be a bread-and butter function that is used by a lot
        of code.

    2)  It should be simple to implement within a runtime system.

    3)  A highly efficient implementation should be possible.

    4)  The edge-cases (off-class inputs and strictness properties)
        should be carefully designed to not complicate implementation.

    These criteria are somewhat coupled with stdlib design and with
    high-performance runtime design.  Since this system will need to be
    finalized before those things are completed, evaluating these designs
    requires some level of "thinking forward" to imagine what those will
    look like.

B.  Jet definitions should be correct, clear, and unsurprising.

    1)  Jets definitions should be *obvious*.  Whenever possible, you
        should look at the definition of a jet and think: "Ah, of course".

    2)  Jets definitions should be small.

    3)  Edge-cases (off-class inputs and strictness properties) should
        have simple, well-defined, unsurprising behavior

C.  Few jets should be mandatory.

    1)  There should be a few "load bearing" jets that the other jets
        are defined in terms of.

    2)  The remaining jets should be defined as usage patterns of these
        "load bearing" jets.

    3)  Implementing the "load bearing" ones should be enough to get a
        basic implementation working in a practically-usable way.

D.  The PLAN code of jets should be fast enough to run on small test
    inputs with the jet disabled.

    1)  Having a simple implementation with few dependencies is more
        important than having fast PLAN code.

    2)  The PLAN code backing jets does not need to have good asymptotics.

    3)  However, jet code should not have "resonable" implementations
        that can be run unjetted for testing purposed.

    These concerns are especially important when trying to bootstrap
    a new runtime system, and also make validating that jet
    implementations match their formal definitions easier.

E.  The jets should work together so that the *system of jets* satisfies
    these properties.

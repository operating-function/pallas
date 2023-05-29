Tabs and Cabs
=============

This document describes the rational for the design of the legal code
for tabs and cabs.

The design of data-jet representation is tricky, because there are
a number of different design criteria that are somewhat in conflict,
and yet the final answers need to be "correct".

This document attempt to explain those criteria and to discuss how the
data-jetted representation for cabs and tabs fit into those requirements.


Data Jet Design Criteria
------------------------

1)  Disassembled PLAN code should be as straightforwards and "obvious"
    as possible.  Auditing disassembled PLAN binaries should be easy.

2)  Implementations are expected to replace these functions with internal
    primops, so the efficiency of the legal code is often not important.

3)  The nouns underlying data jets should be small, especially
    when serialized.  The standard Save/Load routines do not understand
    data jets.

4)  Matching data-jets should be fast.

5)  Data-jet shapes should not overlap with patterns used by user code.

    User-defined functions should fail the jet-match test immediatly,
    without needing to traverse law bodies.

6)  Data-jet shapes should not overlap with other data-jet shapes.
    The decision about which data-jet is a potential match should be
    very fast.

7)  If data jets are used as PLAN values (pattern matching, using them
    as a function), that behavior should be easy to implement correctly and
    that should be reasonably performant.

8)  The underlying legal code does not need to have the same space or
    time complexity as the jetted versions, but there should be clear
    guidelines on what can be expected of the optimized version. For
    example, "indexing into a row is very fast an has O(1) time
    complexity."

9)  At small scales, performance should be "reasonable" even without the
    jets. Being able to run code without the jets is great for
    validation. Also, when building new implementations you want to be
    able to test things before everything is in place. Finally, very
    minimalistic implementations may want to omit certain jets
    altogether.


The PLAN Representation of Cabs and Tabs
----------------------------------------

The data structure underlying a cab is a sorted row without duplicates.
Cabs are laws with name=1 arity=(size + 1) and body=(the row).
For example:

    %[a b]

        = (0 0 3 [%a %b])
        = (0 0 3 (0 0 3 0 %b %a))

Tabs are represented as a cab partially-applied to the corresponding
values (in descending order).  This matches the approach used by rows:

    #[a=3 b=4]

        = (%[a b] 4 3)
        = (0 0 3 [%a %b] 4 3)
        = (0 0 3 (0 0 3 0 %b %a) 4 3)


Design Evaluation
-----------------

The nouns underlying tabs and cabs are small and compress well.

For example `[%[a=1 b=2] %[a=3 b=4]]` serializes as:

    x=(0 0 3)
    v=(x 0)
    ab=(x (v %b %a))
    (v (ab 2 l) (ba 4 4))

Which takes only a handful of extra bits beyond what is required for
the keys and values.

TODO: Discuss the actual operations and how well those work in different
implementation settings (without any jets, with a minimal set of jets,
with a maximal set of jets).


Potential Improvements
----------------------

### The Legal Name of Cabs

First, the zero tag on cabs is probably the wrong choice, as it does
overlap with the space of reasonable user-defined laws.

This tab, for example:

    (0 0 3 0 [%a %b])

Is equivalent to this functions:

    `(_ _ _ & [0 1])`

Furthermore, this function requires the jet-matcher to traverse the law
body, even though it isn't actually a cab.

    `(_ _ _ & [1 0])`

    Cabs should probably instead have the tag `1`.

        %[a b]
            = (0 1 3 [%a %b])
            = (0 1 3 (0 0 3 0 %a %b))

Cabs should probably instead use `1` as the law name.


### Passing Values to Tabs

In runtime systems that jet rows, but not tabs, the current tab
representation is fairly inefficient, and leaving off these jets in a
hyper-minimalistic bootstrapping runtime probably makes sense.

Furthermore:

-   This would eliminate the annoying edge-case where an empty cab is
    the same as the empty tab.

-   Cab matching would be simpler because the size
    check goes away.

-   Raw pattern matching on tabs becomes simpler and cheaper (it's just
    an APP node that is a pair of the keyset and the values array).

For example, that would look like:

    %[a b]
        = (%[a b] [1 2])
        = ((0 1 2 [%a %b]) [1 2])
        = (0 1 2 (0 0 3 0 %b %a) (0 0 3 0 2 1))

This is slightly larger than the previous structure, but compresses
very well.  The vector-constructor is always shared between the keys and
the values, and the cab constructor prefix `(0 1 2)` is always shared
(does not vary by size).

`[%[a=1] %[b=2 c=3]]` serializes as:

    zz=(0 0)
    v2=(zz 3 0)
    c=(0 1 2)
    v1=(zz 2 0)
    (v1 (c (v2 %c %b) (v2 3 2)) (c (v1 %a) (v1 1)))

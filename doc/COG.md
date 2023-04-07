Machine
-------

In the plunder VM, a "Machine" is a set of persistent processes, which
are referred to as Cogs.

Cogs can survive restarts. Their inputs are written to an event log, and
their state is occasionally snapshotted to disk. On restart, we recover
the most recent state by loading the most recent snapshot and passing in
each input.

Cogs interact with the world by making system calls. The set of active
system calls made by Cogs is a part of its state. When it is reloaded,
all of the calls will be resumed as well.

There is no hidden state in machine. A machine can shutdown and resumed
without an visible effect.

Cogs
----

A cog is a plunder function partially applied to a row of syscalls.

    ($cog [[%eval 0 add 2 3] [%rand 0 %byte 8]])

An event is given to a cog by calling it as a function. Each event is a
set of syscall responses `(Map Nat Any)`:

    | ($cog [[%eval 10 add 2 3] [%rand 0 %byte 8]])
    | %% =0 [5]
    | %% =1 x#0011223344556677

System calls like `%rand` are CALLs to hardware.

-   The `0` is the "durability" flag that tells the ships that the
    effect should not be executed until the input that lead to the
    effect has been commited to the log.

-   `[%byte 8]` is the argument-list that is passed to the `%rand`
    device.

`%eval` is the one special case. `%eval` asks for plunder code to be
evaluated asynchronously.  The result is that we can take advantage
of parallelism, and that the main loop is not slowed down when the Cog
needs to perform an expensive computation.

-   The `10` in `[%eval 10 add 2 3]` is an upper-bound on the number of
    seconds that an evaluation is allowed to run for. An evaluation that
    takes longer than that is canceled.

-   The `[add 2 3]` indicates that EVAL should evaluate the expression
    (add 2 3).

-   The reason that `%eval` is special, is because the event log does
    not actually contain the result of an EVAL call, instead the event
    log simply records that the event succeeded, and the result is
    re-calculated on replay.

-   This is important because it means that extremely large values can
    be returned by EVAL without bogging down the log.

<!---
Local Variables:
fill-column: 73
End:
-->

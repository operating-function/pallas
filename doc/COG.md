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

There is no hidden state in a machine. A machine can shutdown and resume
without a visible effect. The formal state of a full Machine is the
plunder value `(Tab Nat Fan)`, where each cog has a process id mapping to
its formal state.

Cogs
----

A cog is a plunder function partially applied to a row of syscalls.

    ($cog [[%eval 0 add 2 3] [%rand 0 %byte 8]])

An event is given to a cog by calling it as a function. Each event is a
set of syscall responses `(Map Nat Any)`:

    | ($cog [[%eval 10 add 2 3] [%rand 0 %byte 8]])
    | %% =0 [5]
    | %% =1 x#0011223344556677

There are four types of syscalls: `%eval` requests, `%cog` requests,
`%what` requests to detect the attached hardware and everything else is
treated as a CALL to hardware. The interpreter ignores any value that it
does not understand.

A CALL to hardware looks like `[%rand 0 %byte 8]`. Breaking it down:

-   `%rand` is the name of the hardware targeted.

-   The `0` is the "durability" flag that tells the ships that the
    effect should not be executed until the input that lead to the
    effect has been committed to the log.

-   `[%byte 8]` is the argument-list that is passed to the `%rand`
    device.

If the current plunder VM does not have a piece of hardware given the
passed in name, no attempt to handle the request will be made.

That's why we need `%what` to synchronize what the cog thinks are the
callable pieces of hardware. A cog doesn't detect when the plunder
interpreter has been restarted or replaced with a different one and must
know about what current capabilities are provided by CALL. At first, a
cog issues a `[%what %[]]` request, receives a `%[%rand %http]` response
and then holds open a `[%what %[%rand %http]]` request which will only
change if the interpreter does.

`%eval` asks for plunder code to be evaluated asynchronously.  The result
is that we can take advantage of parallelism, and that the main loop is
not slowed down when the Cog needs to perform an expensive computation.

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

Finally, there are the `%cog` requests. A user is likely to have multiple
processes that they wish to run, and having those processes communicate
over hardware CALLs would mean that each IPC message must be written into
the event log. So we have a few special calls for process management and
IPC between cogs. Like `%eval`, most `%cog` requests have special event
log representations so that you're storing a record that something
happened that could be recalculated on log replay.

(If cog A sends a message to cog B, all you need to do is record that B
processed the message from cog A at a given request index, instead of
serializing and storing the full noun sent in the event log.)

The `%cog` requests are:

-   `[%cog %spin fan] -> IO Pid`: Starts a cog and returns its cog id.

-   `[%cog %send pid channel fan] -> IO Nat`: Sends the fan value to pid
    on a channel, returning afterwards. `%send`/`%recv` operate on word64
    channels, which allow two different cogs to maintain separate message
    streams that don't interact with each other. `%recv`/`%send` are
    special in that they always both execute atomically; you'll never
    have one without the other in the event log.

    The returned nat is 0 if the message was sent successfully, and 1 if
    the cog crashed.

-   `[%cog %recv channel] -> IO (Pid, Fan)`: Waits for a message to be
    sent on to this cog on the given numeric channel.

-   `[%cog %stop pid] -> IO (Maybe CogState)`: If the cog does not exist,
    immediately returns None. Otherwise, stops and removes the cog from
    the set of cogs and returns the `CogState` value.

-   `[%cog %reap pid] -> IO (Maybe CogState)`: If the cog does not exist,
    immediately returns None. Otherwise, waits for a cog to enter an
    error state, removes the cog from the set of cogs and returns the
    `CogState`.

    (In the case where there's a %reap and a %stop open, the calling
    `%stop` takes precedent and receives the cog value, and the `%reap`
    receives None.)

-   `[%cog %who] -> IO Pid`: Tells the cog who it is. Any other way of
    implementing this would end up with changes to the type of the cog
    function taking an extra `Pid ->`.

The on disk snapshot of a whole Machine is just the noun value of `(Tab
Pid CogState)` serialized, where Pid is a natural number and `CogState`
is a row matching one of the following patterns:

-   `[0 fan]`: represents a spinning cog which has requests and can
    process responses.

-   `[1 fan]`: represents a finished cog, a cog which shut down cleanly
    by having no requests, so it will never receive a response in the
    future.

-   `[2 (op : nat) (arg : fan) (final : fan)]`: represents a crashed cog,
    with the `op` and `arg` being the values that caused the crash and
    `final` being the final value of the cog before the crashing event.

-   `[3 (duration : nat) (final : fan)]`: represents a cog which had a
    request timeout.

These patterns are also what are returned in the `[%cog %stop]` and
`[%cog %reap]` requests.

<!---
Local Variables:
fill-column: 73
End:
-->

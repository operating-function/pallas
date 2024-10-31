System Calls
============

    # data Worker
    - EVAL (Row Any)  --  Concurent evaluation (function + args)
    - EXEC Proc       --  Worker process

    type Db = ( st                         --  state
             -> (st -> Nat -> Any -> Any)  --  pure reads
             -> Row Worker                 --  eval/exec workers
             -> Row (Nat, Any)             --  input
             -> (Db, Row (Nat, Word, Any)) --  new state, outputs
              )

       -- All "outputs" are messages to a worker process, the Nat of an
       -- output corresponds to an index into the "workers" array.
       -- Outputs that do not correspond to a valid worker are just ignored.

       -- EXEC workers are kept running forever.  On machine-boot or
       -- worker-crash, the process is simply restarted.  There is no
       -- database event corresponding to either of these situations,
       -- but such behavior could be implemented by having a {Proc}
       -- start by informing the database that it has restarted.

    type Proc = Row (Word, Ef a) -> Word -> a -> Proc

        -- The {Word} here is something like a "request id".  It has no
        -- semantics except that every response is tagged with the same
        -- word that the request was tagged with.
        --
        -- The `a`s here don't all have to line up. The type variable
        -- is used here just to indicate that the final param is the
        -- response type of an effect.

    type Conn = Nat
    type Port = Nat
    type IP   = Nat

    data Ef a where
        DB_WAIT        : Ef Any
        DB_READ        : Any -> Ef Any
        DB_WRITE       : Any -> Ef Void
        TIME_WHEN      : Ef Nat
        TIME_WAIT      : Nat -> Ef Unit
        TCP_OPEN       : IP -> Port -> Ef Conn
        TCP_GIVE       : Conn -> Bar -> Ef Nat
        TCP_HEAR       : Ef (Conn IP, Port)
        TCP_TAKE       : Conn -> Ef Bar
        TCP_PORT       : Ef Nat
        TCP_SHUT       : Conn -> Ef Unit



An cog (database) input is either an evaluation result or a write
transaction from a worker, the Nat indicates which worker triggered
the input.

An cog (database) output is a message to a worker process (typically in
response to a command).

Example:

-   First, we launch an HTTP server on a worker process (with a EXEC
    request).

-   That HTTP server-worker receives a GET request.

-   The worker parses the request and then submits it to the SSI via an
    `DB_READ` effect, the request is satisfied by calling the "pure reads"
    function without hitting the SSI.

-   The worker receives the result of this evaluation.

-   The worker uses this information to assemble a response, and then
    does a series of socket effects in order to send the HTTP response.

-   A PUT request comes in.

-   The worker parse the request and then submits it to ths SSI via
    running a `DB_WRITE` effect (command).

-   The worker also issues a `DB_WAIT` effect to receive the result of
    the write transaction.

-   The SSI receives the command as an input, and then launches an EVAL
    worker to calculate the state update.

-   The EVAL worker finishes, and the SSI updates it's state and sends
    a message to the worker with the result.

-   The worker gets this input from the database and uses that to respond
    to the PUT request.

This interaction requires the following event-log writes:

-   A `DB_WRITE` command from the worker:

        (0 (%write "/hi.txt" "Hello World!"))

-   A successful eval result.

        1

The thing to note here is that the vast majority of the activity here
does not require SSI/disk activity.

Ignore this
===========

This is just sketching out how the memory layout of the event log might
look, if the event log is stored in the same way as a normal persistent
heap.

The event log for this whole interaction would then be just 10 words:

    [0]: "Hello Wo"
    [1]: "rld!"
    [2]: %write
    [3]: "/hi.txt"
    [4]: BIG(@0, sz=12)
    [5]: ROW(@2, tag=0, sz=3)
    [6]: 0
    [7]: ROW(@5, tag=0, sz=2)     ;; first event
    [8]: 1
    [9]: ROW(@7, tag=0, sz=2)     ;; second event

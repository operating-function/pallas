# New SysCall Proposals

## %net as the Sole Hardware Device

I propose that we eliminate all hardware devices besides %net.

With this change, "eval", "cog", and "net" are the only effect types.
"wait" and "rand" become %net effects, and all %net effects return
a timestamp.

In addition to being a major simplification, this move would get us very
close to achieving a totally finalized hardware interface.

I argue that %net is the only hardware interface that we need, anything
else that could potentially be a hardware device can instead just be a
server that a cog interacts with via %net.

If these services are available on the same machine, there is only a
small amount of overhead required for doing the interaction via %net,
and this has the advantage that the "hardware device" can be moved around,
it is not tied to the same physical machine as the database.

The only cases where this wouldn't work well, are for devices that require
low-latency interaction, where the %net overhead is too much.  However,
the SSI model is generally bad at low-latency stateful stuff anyways,
so there's no point in having such devices.

For low-latency stuff we need to do the "run a program on a device and
talk to it" model, and that can be done perfectly well over %net.

## A Better SysCall Interface

Instead of providing direct read/write operations on sockets, we have
an concept of a "socket agent", where the database essentially says
"run this program that interacts with these sockets"

This is kinda analogous to the UI paradigm.  Instead of direct interaction
with graphics/mouse, you say "here's a HTML/JS program".  It will deal
with those things and then I'll just talk to it.

The cog ends up being essentially just a database that builds programs,
runs them against various devices, and coordinates them.



    ;;; Ports, Sockets, Servers, and Agents ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    type PortName = Word64
    type Addr     = Ed25519.PublicKey
    type Secret   = Ed25519.PrivateKey
    type Port     = (Addr, PortName)

    ;;; {claim} never returns unless Secret doesn't match Addr.
    ;;; A valid {claim} makes the addr available to all cogs
    ;;; in the machine, software mechanisms needed for access
    ;;; control.
    ;;;
    ;;; {serve} set (installs, replaces, or removes) the server
    ;;; that's running on a {Port} {serve} returns NONE if
    ;;; the server is being removed, or if there is no valid
    ;;; {claim} running against the {Addr} in the given {Port}.
    ;;;
    ;;; {accept} only returns NONE if the server has been
    ;;; replaced (is no longer running against a Port), and
    ;;; has no pending connections (or if there is no server
    ;;; with this handle).  If there are ever 0 {accept} calls
    ;;; against a {ServerHandle}, the server is killed.
    ;;;
    ;;; {deploy} runs a socket agent, which can make
    ;;; connections to various ports.  This returns SOME once
    ;;; an agent has privided it's first "give".
    ;;;
    ;;; {send} sends a message to an agent (which will receive
    ;;; it using the {take} syscall"
    ;;;
    ;;; returns FALSE if there is no such agent, otherwise
    ;;; returns TRUE.
    ;;;
    ;;; {recv} receives a message from an agent (which provides
    ;;; the value using the {give} syscall).  A call to {recv}
    ;;; returns NONE if there is no such agent, otherwise
    ;;; returns the received value.

    claim : Addr -> Secret -> IO ()

    serve : Port -> Opt (Socket -> Agent) -> IO (Opt ServerHandle)

    accept : ServerHandle -> IO (Opt (Any, AgentHandle))

    deploy : Agent -> Opt (Any, AgentHandle)

    send : AgentHandle -> IO Bit

    recv : AgentHandle -> IO (Opt Any)


    ;;; Agent Effects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; An `Agent` is a cog specifically for interacting with Sockets.
    ;;; Agents have a very limmited number of effects and are not persisted.
    ;;;
    ;;; {connect} opens a connection from an address to a port, returning
    ;;; NONE if the system failed to open the connection.
    ;;;
    ;;; {read} reads {Nat} bytes from a socket.  Returns an
    ;;; empty Bar if the socket has closed.
    ;;;
    ;;; {write} writes bytes from the given {Bar} to a socket, it
    ;;; returns 0 if the socket has closed, otherwise it returns the
    ;;; number of bytes written.
    ;;;
    ;;; {give} makes a value available to the database, and {take}
    ;;; receives a value from the database.
    ;;;
    ;;; {sleep} waits for some milliseconds.

    connect : Addr -> Port -> IO (Opt Socket)

    read : Socket -> Nat -> IO Bar

    write : Socket -> Bar -> IO Nat

    give : Any -> IO ()

    take : IO Any

    sleep : Nat -> IO ()


## Flattening the SysCall Namespace

If both of these approaches work out, it might make sense to even
eliminate the three "namespaces" for effects, and just have a unique
name per SysCall.

A cog would have something like this set of effects:

    %eval
    %who %spin %stop %reap %send %recv
    %wait %rand
    %claim %serve %accept %deploy %give %take

And an agent would have something like:

    %open %read %write %give %take %sleep

Details TBD.

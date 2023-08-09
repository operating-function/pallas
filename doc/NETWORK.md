(TODO: This document is written with an Urbit audience in mind, and
should probably be generalized later).

(TODO: This is a very rough draft, revise this)

# Plunder Networking

The Plunder network model is the last big piece of the system that is
not yet finalized.

Because we haven't produced a serious implementation yet, it's possible
that some of the design choices presented in this document will need to
be revisited based on issues discovered along the way.

However, the basic *idea* of how Plunder networking should work has been
stable for a while now.

The are basically two pieces, `port`, and `book`.

## Addresses

Plunder uses Ed25519 signing keys as network addresses.

Plunder cogs don't necessarily have a single address. They can have
multiple addresses, or zero addresses. Addresses can be long-lived or
ephemeral.

The name resolution system (Azimuth, DNS) is in a separate layer and is
not discussed here.

## Port

Port is just message delivery.

Syscalls:

    port.send :: us/KeyPair -> them/Address       -> ByteString -> IO ()
    port.recv :: us/KeyPair -> them/(Set Address) -> IO (Address, ByteString)

This is somewhere in between Ames and UDP.

-   Ames provides encrypted, arbitrary size, exactly-once-delivery
    messages of any size to semantic addresses.

-   UDP provides size-limited messages to physical addresses, with no
    delivery guarantees.

-   Port provides encrypted, arbitrary size, messages to
    somewhat-semantic addresses. It guarantees at-least-once-delivery,
    but does not guarantee that the remote host successfully processed
    the message, just that it was physically received.

Port gets as close to Ames as possible without requiring any persistent
state. This means that most of the fiddly networking stuff can live
within the runtime subsystem. It also means that the details of network
transport can evolve without breaking software.

Software is still responsible for ensuring delivery, for serialization,
for managing channels, for associating names with addresses, etc.

Our initial implementation of Port will probably just be over HTTP. This
is inefficient, but allows us to just rip off existing software instead
of building everything from scratch.

We can create more sophisticated transports later, without changing
software.

## Book

Book is just "file sharing" for arbitrary nouns.

The overall goal is to make it easy to share large datasets with
friends, and to make it easy to publish arbitrary data.

This functionality should be fully integrated into everything. If you
produced an album or recorded a video, you should be able to share that
data directly within a chat conversation, and interact with that object
collaboratively.

There should be no restrictions on sharing, and you shouldn't need to
depend on some external service to share data.

This sharing should be invisible to everyone besides the parties
involved. There should be no surveillance, censorship, or moderation
outside of what is imposed socially by the group in question.

### Book Design

The BOOK hardware takes advantage of the fact that we have already
broken our data up into "pages" using the Pin system (the architecture
requires this for efficient snapshotting).

This "Pin DAG" structure conveniently maps exactly to the structure
needed by the BitTorrent algorithm. BitTorrent needs data to be broken
up into chunks and encoded as a Merkle DAG, but all Plunder data is
already encoded that way.

We take advantage of this to basically offer "torrents" as a primitive
construct. This allows a cog to publish static data, and have the actual
serving of that data be done by others (either by other users, or by
reseeding services).

We intend to use an encryption scheme for each node, so reseed services
don't need to know what they are serving. They are just commoditized
services that serve encrypted blocks.

This also makes many networked things possible with cogs that are mostly
offline. You can publish or share data from a laptop, and have it still
be available when the laptop goes to sleep.

Syscalls:

    -- Serve a book.
    book.serve : us/KeyPair -> them/(Set Address) -> Noun -> IO Void

    -- Download the whole book.
    book.fetch : us/KeyPair -> book/Address -> IO Noun

    -- Tell me if the contents changed.
    book.watch : us/KeyPair -> Hash256 -> IO Hash256

    -- Keep a local mirror and reseed for others.  Return if the contents
    -- change.
    book.reseed : us/KeyPair -> book/Address -> Hash256 -> IO Hash256

    -- Download an index of a book (Set of pins, their hashes and edgelists)
    book.index
        :: us/KeyPair
        -> book/Address
        -> hash/Hash256
        -> IO (Vector (Hash256, List Nat))

    -- Download a single pin from a book, needs to be deserialized
    -- explicitly.
    book.pick : us/KeyPair -> book/Address -> Hash256 -> IO ByteString

The reason why we have the `pick` request, is so that very large
datasets can be shared as one `book`.

For example, a photographer could share their entire portfolio. You
would store a searchable index, a collection of thumbnails, and the full
collection of full-sized photos, all as one value.

The client would:

-   Download just the search index
-   Perform searches, loading thumbnails on demand
-   Download selected full-size photos.

### Initial Book Implementation

The full vision for the BOOK hardware requires a lot of engineering
work. BitTorrent is a solved problem, but it is not easy to implement
well.

As a short-term solution, clients will use HTTP to download blocks,
either directly from the cog that serves them, or from the cog's router,
if it is re-seeding the data.

The implementation of the protocol can evolve over time without changing
the interface, and without breaking software.

# PLAN Seeds

This is a simple format for serializing (normalized) PLAN trees.

This format is optimized for simplicity of decoding, not speed or size.

We convert everything to atoms and cells via `(<p> -> (4 p))` and
`({n a b} -> (4 n a b))`.  The original values can be recovered via
evaluation.

In particular, this should only be used for "trustworthy" inputs.
Since decoders are stupid, and since complex values are reconstructed
via evaluation, it is possible to create seeds that contain computation,
and those computations may diverge.

For example, you can encode the value `(0 0 0 0)`, which trivially
diverges.

All atoms are de-duplicated and stored in ascending order.

All cells are de-duplicated by serializing trees as a series of
tree-fragments, each of which may refer to any earlier fragment.


# The Format

Seed files have the following layout:

    u64                numHoles
    u64                numBigs
    u64                numWords
    u64                numBytes
    u64                numFrags
    u64[numBigs]       bigSizes
    u64[sum(bigSizes)] bigData
    u64[numWords]      words
    u8[numBytes]       bytes
    u64[]              fragData  // a series of bit-encoded fragments

Decode item-by-item, appending each to an accumulator.

Each fragment is bit-encoded with the following scheme:

    1xy -> (x y)
    0n  -> accumulator[n]

Where the bit-width of `n` is the number of bits required to store the
biggest possible index.  If there are four elements in the accumulator,
each index is two bits, etc.

    indexBitWidth = ceil(log2(accumulator.length())

The atoms are stored in decreasing order so that we don't need padding
after the bytes to maintain word-alignment.


# Encoding

## De-duplication

It's easy enough to de-duplicate atoms, just insert them all into a
hash table.

However, using the same approach for trees is expensive, since all
operations are O(n) on the size of each tree, and trees can be quite
large.  Hashing and equality are both O(n) for each node.

So, we traverse the tree depth-first, and assign an index to each
unique node that we find.

Then we represent each cell as an `(Idx, Idx)` pair and use that as the
value to inserted into the de-duplication table for cells.

For example, to de-duplicate the value `((1 2) (1 2))`.

    ((1 2) (1 2))  []                         {}
    ((a 2) (1 2))  [a=1]                      {1=a}
    ((a b) (1 2))  [a=1 b=2]                  {1=a 2=b}
    (c     (1 2))  [a=1 b=2 c=(a b)]          {1=a 2=b (a b)=c}
    (c     (a 2))  [a=1 b=2 c=(a b)]          {1=a 2=b (a b)=c}
    (c     (a b))  [a=1 b=2 c=(a b)]          {1=a 2=b (a b)=c}
    (c     c    )  [a=1 b=2 c=(a b)]          {1=a 2=b (a b)=c}
    d              [a=1 b=2 c=(a b) d=(c c)]  {1=a 2=b (a b)=c (c c)=d}

For the next thing, the shatter() algorithm, we also need to track the
number of references on each node.


## Converting De-duplicated Trees into Fragment Sequences

See [shatter.txt].


## Sorting Atoms

We have an array of unique atoms, in traversal order.  However, the
numbers need to be serialized in descending order, and back-references
to numbers are also in sort-order.

We can't just sort our array of numbers, since our cells contain indexes
into the traversal-order array.

So, we need to derive the sort-order for this array, and we need to
produce a mapping from traversal-order-index into sort-order-index
(for back-references into the arrays table).


# Single-Pin Output

The same format can be used with a header, to encode single pins.

Just add this header:

    u64           numPins
    u256[numPins] pins     (traversal order)

And then increase each index in each fragment by `numPins`.

# Seed files

::::warning

TODO: Explanation of seed files and serializiation.

* what are seed files, how are they used in the boot process?
* brief overview of the serialization/deserialization algorithms
* what does it mean for a ship to boot from a seed file?
  * (or a REPL, for that matter)

For now: [https://git.sr.ht/\~plan/plunder/tree/master/item/doc/seed](https://git.sr.ht/\~plan/plunder/tree/master/item/doc/seed)

::::

## PLAN Seeds

Seed is a simple format for encoding binary trees of natural numbers.

The format has a number of desirable properties:

* It's very simple to write an efficient SEED loader.
*   Naturals are stored word-aligned and word-padded. Multi-word naturals can be loaded with zero copies by just storing big numbers as pointer into the input buffer.

    This is especially useful when working with files that are loaded with mmap() and contain large binary blobs. In this situation, the OS will transparently page this data in-and-out as necessary.
* Individual DAG nodes of a PLAN value can be serialized individually. For example, by using hashes to encode an edge-list.
* All repeated subtrees are written only once.

When serializing PLAN values, we write out pins and laws by encoding them as expressions that evaluate to pins and laws. For example `<3>` is encoded as `(4 3)`. `{"_ToNat" 1 (0 (2 0 3) 1)}` is encoded as `(0 127961276568671 1 (0 (2 0 3) 1))`.

PLAN trees encoded with seed can contain _computations_, not just values. If this is undesirable, the decoder can just verify that everything it reads is in normal form.

All atoms are de-duplicated and stored in ascending order.

All cells are de-duplicated by serializing trees as a series of tree-fragments, each of which may refer to any earlier fragment.

## The Format

At a high-level, the format is:

* Size metadata.
* All of the numbers, in descending order.
* Tree data.
* We order the atoms so that they can be binned by size bignats, words, and bytes. This avoids the need for complex encoding of size metadata.
* We use descending order so that bytes are written last, otherwise we would need padding after the bytes to maintain word-alignment.

For sharing, tree data is encoded as a series of tree "fragments" each of which can reference fragments that came before it. For example, the value ((0 1) (0 1)) is stored as:

```
[0]: 1
[1]: 0
[2]: ($1 $0)
[3]: ($2 $2)
```

More precisely, seed files have the following layout:

```
u64                   numHoles    // number of external references
u64                   numBigNats  // number of nats bigger than one word
u64                   numWords    // number of nats bigger than one byte
u64                   numBytes    // number of nats that fit in a byte
u64                   numTrees    // number of tree fragments
u64[numBigNats]       bigNatSizes // word-size of each multi-word nat
u64[sum(bigNatSizes)] bigNatData  // actual data for each multi-word nat
u64[numWords]         words       // actual data for each word-sized nat
u8[numBytes]          bytes       // actual data for each byte-sized nat
u8[]                  trees       // bit-encoded binary trees
u8[]                  zeros       // zeros to make the size a multiple of 8
```

Each tree is bit-encoded with the following scheme:

```
1xy     -> (x y)
0(r{n}) -> where n is the bit-width of the maximum backref size.
```

each element of the `trees` array omits the outermost 1 bit. Since each atom is already in the table, the top of every `tree` is always an interior node.

For example, `((0 1) (0 1))` is this data:

```
[0]: 1
[1]: 0
[2]: ($1 $0)
[3]: ($2 $2)
```

Which is stored as:

```
numHoles=0
numBigNats=0
numWords=0
numBytes=2
numTrees=2
[bigNatSizes]
[bigNatData]
[words]
[bytes]
0b00000001
0b00000000
[trees]
{01}{00}         // two possible references = 1 bit per leaf
{010}{010}       // three possible references = 2 bits per leaf.
[zeros]
00000000000000000000000000000000000000
```

Or, in raw binary, least-significant bit-first:

```
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000010
0000000000000000000000000000000000000000000000000000000000000010
1000000000000000010001001000000000000000000000000000000000000000
```

## Decoding

* The head has a fixed size, so we can simply copy those bytes into words.
* Once we have loaded the header, we can determine the total size of the table used during load.
* Load all of the big-nats, load all of the words, load all of the bytes.
*   Read the remaining file bit-by-bit decoding the tree structure.

    indexBitWidth = ceil(log2(accumulator.length())

The atoms are stored in decreasing order so that we don't need padding after the bytes to maintain word-alignment.

## Encoding

### De-duplication

Deduplication for atoms is easy: just insert each atom into a hash table.

However, using the same approach for trees is expensive.

Urbit's `jam` solves this by putting a lazily-calculated hash value on every node, that solves the O(n) cost of hashing each node, and it also solves the O(n) cost of comparing trees, (since deep comparisons of similar but non-equal trees will quickly find a non-matching hash).

However, the `jam` approach increases the memory-footprint of each cell by 50%.

Seed uses a different approach that avoids the per-node hash at the price of needing to do a full depth-first traversal of the DAG. This doesn't scale to DAGs with massive sharing, but that problem is already solved with pins.

The approach that seed uses, is to write each cell into a table, and to assign each unique cell a unique key. Then we can just store each cell as (key, key) in a de-duplication table. Pairs of keys are efficient to hash and efficient to check for equality.

For example, to de-duplicate the value `((1 2) (1 2))`.

```
((1 2) (1 2))  []                         {}
((a 2) (1 2))  [a=1]                      {1=a}
((a b) (1 2))  [a=1 b=2]                  {1=a 2=b}
(c     (1 2))  [a=1 b=2 c=(a b)]          {1=a 2=b (a b)=c}
(c     (a 2))  [a=1 b=2 c=(a b)]          {1=a 2=b (a b)=c}
(c     (a b))  [a=1 b=2 c=(a b)]          {1=a 2=b (a b)=c}
(c     c    )  [a=1 b=2 c=(a b)]          {1=a 2=b (a b)=c}
d              [a=1 b=2 c=(a b) d=(c c)]  {1=a 2=b (a b)=c (c c)=d}
```

### Converting De-duplicated Trees into Fragment Sequences

The process described above creates a table of unique DAG nodes, but we still need to turn that into a series of fragments with back-references.

The trick there is to track the "refcount" of each node during our depth-first traversal. Whenever a reference b, but refcount(a) < refcount(b), b should be written out as a separate tree fragment.

See shatter.txt for the details of that algorithm.

### Sorting Atoms

We have an array of unique atoms, in traversal order.

However, the atoms needs to be serialized in descending order (this is what makes the (bignats, then words, then bytes) encoding possible.

We can't just sort our array of numbers, since our our cells table contains references into that array by index.

So, we build a table of indexes into that array and we sort that.

### Encoding Individual Pins

A single pin (a single DAG-node in the pin DAG) can be stored using a scheme such as:

```
u64           numPins
u256[numPins] pinRefs (in traversal order)
seed(with numHoles=numPins)
```

To load this:

* Read the pinRefs table to get the edgelist.
* Load each dependency.
* Decode the seed file, with the refs-table pre-populated with these pins.

### Seed Pods: Encoding Large pin DAGs

The SEED de-duplication system does not scale to huge, highly duplicated trees, so very large values must be broken into pins and stored separately.

The seed-pod format works by traversing the pin DAG, saving each pin with seed, and adding it to an array.

For example, the DAG:

```
   [a]
  /   \
[b]   [c]
  \   /
   [d]
```

Can be written with the following scheme:

```
SEED(
    [ ([], SEED(d))
    , ([0], SEED(b))
    , ([0], SEED(c))
    , ([1,2], SEED(a))
    ]
)
```

Where the array of numbers is an edge-list (each number being an index into the same array).




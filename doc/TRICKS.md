Dev Tricks
==========

Updating Jet Hashes
-------------------

Updating the jet hashes is annoying, so there's a hacky "trick" for
updating them more easily.

The runtime system outputs Haskell code when it does jet matching.
This output can be caputured from stderr.

    rm sire.cache
    sh/sire-load-all 2>stderr <<<""
    grep ' , e' <stderr >new-jets-table

Then `new-jets-table` can be copied into the big list-literal in
`Fan.JetHash`, and you can review the changes with:

    git diff lib/Fan/JetHash.hs

Make sure that nothing is missing!  If a the name changed, or the jet
name in the code doesn't match the one in the table, it wont appear in
the output at all.

### Detecting Jet Mismatches

Using the same stderr trick, you can check for jet mistmatches by search
for the string "NOT MATCHED".

    grep "NOT MATCHED" stderr

You can also just search back in your terminal history to see if this
shows up anywhere.

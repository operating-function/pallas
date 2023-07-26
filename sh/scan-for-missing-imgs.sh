#!/usr/bin/env bash

FIRST_PAGE=1
LAST_PAGE=20263

DEMOSRC="/home/iceman/src/hitchhiker-haskell/safe-score-gte-37"

for i in {1..20263}; do
    FILE="$DEMOSRC/json/page-$i.json";
    urls=$(cat $FILE | jq -r ".images[] | .representations.thumb")
    for url in $urls; do
        BASEURL=$(dirname $url)
        THUMBFILE=$(basename $url)
        ID=$(basename $BASEURL)

        # Filesystems REALLY don't like directories with hundreds of thousands
        # of files, so on disk we have a two level structure where we cap
        # things at, ie, for an $ID of 1522818, the file is at
        # `1520000/1522818/thumb.gif`.
        if [ $ID -lt "10000" ]
        then BASEDIR="0"
        else BASEDIR="$(expr $ID / 10000)0000"
        fi

        THUMBSRC="$DEMOSRC/img/$BASEDIR/$ID/$THUMBFILE"
        if [ ! -f $THUMBSRC ]; then
            echo "Missing: $ID"
        fi
    done
done

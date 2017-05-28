#!/bin/bash

queued=$(tsp | grep queued | wc -l)
ncpu=$(nproc)

if (($queued > $ncpu))
then
    exit 0
fi

tsp -S $ncpu

SYMBOLS=$(sqlite3 db.sqlite 'select distinct(symbol) from stockprices group by symbol' | shuf)

for i in $SYMBOLS ; do tsp sh/compute.sh $i; done


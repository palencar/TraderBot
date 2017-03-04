#!/bin/bash

queued=$(tsp | grep queued | wc -l)

if (($queued > 32))
then
    exit 0
fi

tsp -S $(nproc)

SYMBOLS=$(sqlite3 db.sqlite 'select distinct(symbol) from stockprices group by symbol' | shuf)

for i in $SYMBOLS ; do tsp sh/compute.sh $i; done


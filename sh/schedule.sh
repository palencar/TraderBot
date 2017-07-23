#!/bin/bash

timeframe=$(printf '1D\n15M\n30M\n' | shuf -n1)
queued=$(tsp | grep queued | wc -l)
ncpu=$(nproc)

if (($queued > 20))
then
    exit 0
fi

list=$(shuf -n 40 list.txt)

for i in $list
do
  if [ $(tsp | egrep -v "queued|running" | grep $i | wc -l) == 0 ];
  then
    echo "Queueing $i $timeframe"

    tsp sh/compute.sh $i 100 $timeframe
  fi
done

tsp -S $ncpu

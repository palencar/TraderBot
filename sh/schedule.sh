#!/bin/bash

ncpu=$(nproc)

#tsp -S $ncpu

queued=$(tsp | grep queued | wc -l)

while (( $queued < 200 )); do

  timeframe=$(printf '1H' | shuf -n1)
  i=$(shuf -n 1 list.txt)

  if [ $(tsp | grep $timeframe | grep \ $i\  | wc -l) == 0 ] && [ $(ls -t result/*.$timeframe.* | head -n 100 | grep $i | wc -l) == 0 ];
  then
    echo "Queueing $i $timeframe"
    tsp sh/compute.sh $i 10 $timeframe
    sleep 1s
  fi

  queued=$(tsp | grep queued | wc -l)

done

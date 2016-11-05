#!/bin/bash

DATE=`date +%Y-%m-%d`

SYMBOLS=`sqlite3 db.sqlite 'select distinct(symbol) from stockprices group by symbol'`

for i in $SYMBOLS ;
do
    tsp Rscript TraderBot.R compute 2009-07-20 $DATE $i
done

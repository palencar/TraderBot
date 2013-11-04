#!/bin/bash


while read line
do
    echo "Baixando http://ichart.finance.yahoo.com/table.csv?s=$line&d=11&e=3&f=2012&g=d&a=0&b=3&c=2000&ignore=.csv"
    wget -O "../data/$line" "http://ichart.finance.yahoo.com/table.csv?s=$line&d=11&e=3&f=2012&g=d&a=0&b=3&c=2000&ignore=.csv"
    tail -n +2 "../data/$line" > "../data/$line.csv"
done < list

#!/bin/bash

while read line
do
echo "#!/gnuplot

set title \"$line\"

#set terminal svg size 1024,600 fname \'Verdana\' fsize 10
#set output \'$line.svg\

set terminal png size 1024,600
set output '$line.png'

" > chart_s.plt

cat chart_a.plt >> chart_s.plt

#echo "fit [ \"2006-01-01\":\"2011-12-31\" ] f(x) \"../data/$line\" using 1:((\$3+\$4+\$5)/3) via a, b, c
echo "fit f(x) \"../data/$line\" using 1:((\$3+\$4+\$5)/3) via a, b, c

stddev_y = sqrt(FIT_WSSR / (FIT_NDF + 1 ))

plot \"../data/$line\" using 1:2:3:4:5 with financebars, f(x) + stddev_y, f(x), f(x) - stddev_y
" >> chart_s.plt
#plot [ \"2006-01-01\":\"2012-12-31\" ] \"../data/$line\" using 1:2:3:4:5 with financebars, f(x) + stddev_y, f(x), f(x) - stddev_y

gnuplot chart_s.plt

done < list


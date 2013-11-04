#!/gnuplot

set title "WEGE3.SA"

#set terminal svg size 1024,600 fname \'Verdana\' fsize 10
#set output \'WEGE3.SA.svg
set terminal png size 1024,600
set output 'WEGE3.SA.png'


set timefmt x "%Y-%m-%d"
set format x "%Y-%m-%d"

set xdata time

set xrange [ "2006-01-01":"2012-12-31" ]

set yrange [ 0 : ]

set datafile separator ","

f(x) = a*x**2 + b*x + c

a = 0.1
b = 0.1
c = 0.1
d = 0.1

FIT_LIMIT = 1e-020


fit f(x) "../data/WEGE3.SA" using 1:(($3+$4+$5)/3) via a, b, c

stddev_y = sqrt(FIT_WSSR / (FIT_NDF + 1 ))

plot "../data/WEGE3.SA" using 1:2:3:4:5 with financebars, f(x) + stddev_y, f(x), f(x) - stddev_y


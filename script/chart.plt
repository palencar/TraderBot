#!/gnuplot

set timefmt x "%Y-%m-%d"
set format x "%Y-%m-%d"

set xdata time

set xrange [ "2006-01-01":"2012-12-31" ]

set yrange [ 0 : ]

set datafile separator ","

f(x) = a*x**2 + b*x + c
#f(x) = a*x**3 + b*x**2 + c*x + d

a = 0.1
b = 0.1
c = 0.1
d = 0.1

FIT_LIMIT = 1e-020
#fit f(x) "TRPL4.SA.csv" using 1:(($3+$4+$5)/3) via a, b, c
##fit f(x) "TRPL4.SA.csv" using 1:(($3+$4+$5)/3) via a, b, c, d
#plot "TRPL4.SA.csv" using 1:2:3:4:5 with financebars, f(x) + 10, f(x), f(x) - 10

fit [ "2006-01-01":"2011-12-31" ] f(x) "CPFE3.SA.csv" using 1:(($3+$4+$5)/3) via a, b, c
#fit f(x) "CPFE3.SA.csv" using 1:(($3+$4+$5)/3) via a, b, c, d

stddev_y = sqrt(FIT_WSSR / (FIT_NDF + 1 ))

plot [ "2006-01-01":"2012-12-31" ] "CPFE3.SA.csv" using 1:2:3:4:5 with financebars, f(x) + stddev_y, f(x), f(x) - stddev_y

#fit f(x) "ELET6.SA.csv" using 1:(($3+$4+$5)/3) via a, b, c
##fit f(x) "ELET6.SA.csv" using 1:(($3+$4+$5)/3) via a, b, c, d
#plot "ELET6.SA.csv" using 1:2:3:4:5 with financebars, f(x) + 10, f(x), f(x) - 10
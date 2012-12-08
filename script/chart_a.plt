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



set term postscript color
set output "haskell-runtime.eps"
set log xy
set size 0.58,0.58
set key left top
set xlabel "constraint graph size (# of nodes)"
set ylabel "time (in seconds)"
set xrange [10:1400]

plot 'runtime.dat' u 1:2 title "graph building time", \
     'runtime.dat' u 1:3 title "ranking time" ls 3

!epstopdf "haskell-runtime.eps"
!evince "haskell-runtime.pdf"


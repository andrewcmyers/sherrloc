set term postscript color
set output "runtime.eps"
set log xy
set size 0.58,0.58
set key left top
set xlabel "constraint graph size (# of nodes)"
set ylabel "time (in seconds)"

plot 'simp.dat'   u 1:2 title "simplified", \
     'nosimp.dat' u 1:2 title "not simplified"

!epstopdf "runtime.eps"
!evince "runtime.eps"


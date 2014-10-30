#
# Stacked histograms by percent
#
set term postscript color solid
set output "Haskell-precision.eps"
set title "Comparison with GHC and Helium"
set key invert reverse Left outside
set yrange [0:100]
set ylabel "% of total"
set grid y
set border 15
set style data histograms
set style histogram rowstacked
set style fill solid border -1
set boxwidth 0.75
#
plot for [i=2:5] 'test.dat' using (100.*column(i)/column(6)):xtic(2) title column(i), \
     '' using ($0-1):(50.*$2/$6):2 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+50.*$3/$6):3 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+100.*$3/$6+50.*$4/$6):4 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+100.*$3/$6+100.*$4/$6+50.*$5/$6):5 with labels notitle

!epstopdf "Haskell-precision.eps"
!evince "Haskell-precision.pdf"

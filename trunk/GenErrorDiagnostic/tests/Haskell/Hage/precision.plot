#
# Stacked histograms by percent
#
set term postscript color
set output "Haskell-precision.eps"
set title "Test"
set key invert reverse Left outside
set yrange [0:228]
set ylabel "% of total"
unset ytics
set grid y
set border 3
set style data histograms
set style histogram rowstacked
set style fill solid 1.00 border -1
set boxwidth 0.75
#
plot for [i=2:5] 'test.dat' using (column(i)):xtic(1) title columnheader(i), \
     '' using 0:($2/2):2 with labels notitle, \
     '' using 0:($2+$3/2):3 with labels notitle, \
     '' using 0:($2+$3+$4/2):4 with labels notitle, \
     '' using 0:($2+$3+$4+$5/2):5 with labels notitle

!epstopdf "Haskell-precision.eps"
!evince "Haskell-precision.pdf"

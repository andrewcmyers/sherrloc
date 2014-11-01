#
# Stacked histograms by percent
#
set term postscript color solid
set output "Small-programs-precision.eps"
set title "Comparison with GHC and Helium"
set key invert reverse Left outside
set size 0.6,0.5
set yrange [0:100]
set ylabel "% of total"
set ytics 10
set grid y
set border 15
set style data histograms
set style histogram rowstacked
set style fill solid border -1
set boxwidth 0.75

color(i) = (i==2)? 'red' : \
           (i==3)? 'orange': \
           (i==4)? 'cyan': \
           (i==5)? 'green':\
           'black'

plot for [i=2:5] 'precision.dat' using (100.*column(i)/column(6)):xtic(1) fillcolor rgb color(i) title column(i), \
     '' using ($0-1):(50.*$2/$6):2 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+50.*$3/$6):3 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+100.*$3/$6+50.*$4/$6):4 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+100.*$3/$6+100.*$4/$6+50.*$5/$6):5 with labels notitle

!epstopdf "Small-programs-precision.eps"
!pdftk Small-programs-precision.pdf cat 1-endeast output Small-programs-precision-rotate.pdf
!mv Small-programs-precision-rotate.pdf Small-programs-precision.pdf
!evince "Small-programs-precision.pdf"

#
# Stacked histograms by percent
#
set term postscript color solid
set output "precision.eps"
set size 0.59,0.5
set yrange [0:100]
set ylabel "% of total" offset 2,-0.5
set ytics 10
set grid y
set border 15
set style data histograms
set style histogram rowstacked
set style fill solid border -1
set boxwidth 0.7

set multiplot
unset key

set lmargin at screen .08
set rmargin at screen .33
set title "Chen's Benchmark"

color(i) = (i==2)? 'red' : \
           (i==3)? 'orange': \
           (i==4)? 'cyan': \
           (i==5)? 'green':\
           'black'

plot for [i=2:5] '../Chen/precision.dat' using (100.*column(i)/column(6)):xtic(1) fillcolor rgb color(i) title column(i), \
     '' using ($0-1):(50.*$2/$6):2 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+50.*$3/$6):3 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+100.*$3/$6+50.*$4/$6):4 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+100.*$3/$6+100.*$4/$6+50.*$5/$6):5 with labels notitle

set lmargin at screen .33
set rmargin at screen .58
set title "Hage's Benchmark"
unset ylabel
set format y ""
set boxwidth 0.75

plot for [i=2:5] 'precision.dat' using (100.*column(i)/column(6)):xtic(1) fillcolor rgb color(i) title column(i), \
     '' using ($0-1):(50.*$2/$6):2 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+50.*$3/$6):3 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+100.*$3/$6+50.*$4/$6):4 with labels notitle, \
     '' using ($0-1):(100.*$2/$6+100.*$3/$6+100.*$4/$6+50.*$5/$6):5 with labels notitle

unset multiplot

!epstopdf "precision.eps"
!pdftk precision.pdf cat 1-endeast output precision-rotate.pdf
!mv precision-rotate.pdf precision.pdf
!evince "precision.pdf"

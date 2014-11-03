set term postscript color
set output "performance.eps"
set log xy
set size 0.58,0.5
set key top left samplen 1 box
set xlabel "program size (LOC)" offset -0.1,0.4
set ylabel "time (in seconds)" offset 2,-0.5
set xrange [5:600]
set xtics add ("600" 600)
set xtics add ("50" 50)
set xtics add ("200" 200)
set xtics add ("400" 400)

plot 'runtime.dat' u 1:2 title "analysis time" ls 1, \
     'runtime.dat' u 1:4 title "ranking time" ls 3

!epstopdf "performance.eps"
!pdftk performance.pdf cat 1-endeast output performance-rotate.pdf
!mv performance-rotate.pdf performance.pdf
!evince "performance.pdf"

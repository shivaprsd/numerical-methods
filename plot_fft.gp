set term qt font "/System/Library/Fonts/Palatino.ttc,12"
set tics font "Palatino,12"
set key font "Palatino,12" samplen 0
set grid

set lmargin 12
set bmargin 4
 
set multiplot layout 2,1
    set offsets graph 0, 0, 0.2, 0.1
    set title "Period-3: time series" font "Palatino,18"
    set xlabel "Number of generations (time) →" font "Palatino,14"
    set ylabel "Population →" font "Palatino,14"
    set xrange [1022:]
    plot 'period-3.txt' i 0 pt 6 lc 2 t "Growth rate = 3.8304"

    set title "Period-3: power spectrum" font "Palatino,18"
    set xlabel "Frequencies (cycles per unit time) →" font "Palatino,14"
    set ylabel "Amplitude →" font "Palatino,14"
    unset xrange
    set key autotitle columnhead
    plot 'period-3.txt' i 1 w l lc 7 lw 1.5 not
unset multiplot

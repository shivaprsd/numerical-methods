set term png size 3200,2400 font "/System/Library/Fonts/Palatino.ttc,42"
set tics font "Palatino,36"
set key font "Palatino,36" samplen 0
set grid

set title "Chaotic region magnified" font "Palatino,54"
set xlabel "Growth rate" font "Palatino,42"
set ylabel "Population" font "Palatino,42"
set lmargin 10
set bmargin 4
set pointsize 0.2

set out 'bif_chaos.png'
plot 'bifurcate.txt' pt 6 lc "#ff333333" not

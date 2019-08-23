set term qt font "/System/Library/Fonts/Palatino.ttc,12"
set tics font "Palatino,12"
set key font "Palatino,12" samplen 0
set grid

set xlabel "Number of generations →" font "Palatino,14"
set ylabel "Population →" font "Palatino,14"
set lmargin 10
set bmargin 4
set yrange [0:1]
set xrange [0.8:]
unset label

#set xrange [:50]
#set title "Extinction" font "Palatino,18"
#plot 'logistic.txt' i 0 u 1:4 pt 6 lc 2 t "Growth rate = 0.4"

#set title "Single-population stable state" font "Palatino,18"
#plot 'logistic.txt' i 1 u 1:4 pt 6 lc 2 t "Growth rate = 2.4"

#set xrange [:20]
#set title "Transients" font "Palatino,18"
#set label 1 "for three different seeds" at graph 0.01, 1.02 font "Palatino,12"
#plot 'logistic.txt' i 1 u 1:2 pt 6 lc 2 t "Growth rate = 2.4",\
#     'logistic.txt' i 1 u 1:3 pt 6 lc 4 not ,\
#     'logistic.txt' i 1 u 1:5 pt 6 lc 7 not
#unset label 1

#set title "Period doubling" font "Palatino,18"
#plot 'logistic.txt' i 2 u 1:4 pt 6 lc 2 t "Growth rate = 3.2"
#
#set xrange [:100]
set yrange [0:1.2]
#set multiplot layout 2,1
#    set title "Asymptotes" font "Palatino,18"
#    set label 1 "for two different seeds" at graph 0.01, 1.05 font "Palatino,12"
#    plot 'logistic.txt' i 4 u 1:5 pt 6 lc 2 t "Growth rate = 3.8304",\
#         'logistic.txt' i 4 u 1:2 pt 6 lc 4 not
#    set xrange [:500]
#    unset title
#    plot 'logistic.txt' i 5 u 1:2 pt 6 lc 4 not,\
#         'logistic.txt' i 5 u 1:5 pt 6 lc 2 t "Growth rate = 1 + √8 + ε"
#    unset label 1
#unset multiplot

#set title "Chaos" font "Palatino,18"
#plot 'logistic.txt' i 3 u 1:4 pt 6 lc 2 t "Growth rate = 3.6"

#unset xrange
#set title "Chaos: dependence on x_0 and μ" font "Palatino,18"
#plot 'logistic.txt' i 7 u 1:4 pt 6 lc 6 t "μ = 4, x_0 = 0.75",\
#     'logistic.txt' i 7 u 1:6 pt 6 lc 2 t "μ = 4, x_0 = 0.75(1 + ε)",\
#     'logistic.txt' i 8 u 1:4 pt 6 lc 7 t "μ = 4(1 - ε), x_0 = 0.75"

set title "Intermittency" font "Palatino,18"
plot 'logistic.txt' i 6 u 1:4 pt 6 lc 2 t "Growth rate = 3.8284"

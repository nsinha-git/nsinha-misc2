set terminal postscript portrait enhanced 
set style line 1 lt 9 lw 9 pt 9 ps 9
set output '/tmp/graphOp.ps'
set style data linespoints
set pointsize 2
plot "/tmp/graph.dat" using 1:2,'' using 1:2:4 w labels

 

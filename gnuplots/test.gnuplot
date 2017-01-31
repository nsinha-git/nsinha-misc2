set terminal jpeg
set style data linespoints
set pointsize 2
#plot "/Users/nsinha/mygithubs/nsinha-misc2/gnuplots/test.dat" using 1:2 with labels

plot "/Users/nsinha/mygithubs/nsinha-misc2/gnuplots/test.dat" using 1:2:3 with   lines lc variable ,'' using 1:2:4 w labels

 

set terminal png size 800,800
set output "script_size.png"

set datafile separator ','
set key top left
set style data histogram
set style histogram cluster gap 1
set style fill solid border rgb "black"
set auto x
set yrange [0:*]
set grid x,y

plot 'script_size.csv' using 2:xtic(1) title col, \
                    '' using 3:xtic(1) title col, \
                    '' using 4:xtic(1) title col, \
                    '' using 5:xtic(1) title col,


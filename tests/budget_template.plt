set terminal png size 800,800
set output "RUN_NAME.png"

# Labels and title
set title "Execution Units"
set xlabel "Language"
set ylabel "CPU"
set y2label "Memory"
set format y "%'.0f"
set yrange [0:*]

# Set y2 axis for Memory
set y2tics
set format y2 "%'.0f"
set ytics nomirror
set y2range [0:*]

# Grid
set style data histograms
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.8
set offset graph -0.17,0,0,0

# Data for plotting
set datafile separator ","
set key inside top right

# Plotting CPU on y axis and Memory on y2 axis
plot 'RUN_NAME.csv' using 2:xtic(1) title "CPU" , \
     '' using 3 axes x1y2 title "Memory"

#!/usr/bin/env Rscript
# LOAD DATA
# Data for bar chart
x = c(12, 4, 21, 17, 13, 9)
# BARPLOT WITH DEFAULT COLORS
# Default barplot
barplot(x)

# Color by name.
barplot(x, col = "slategray3") 
# slategray3 is 602 in the list.
barplot(x, col = colors() [602]) 
# RGB hex code.
barplot(x, col = "#9FB6CD") 
# RGB 0-255
barplot(x, col = rgb(159, 182, 205, max = 255)) 
# RGB 0.00-1.00
barplot(x, col = rgb(.62, .71, .80)) 
# BARPLOT WITH BUILT-IN PALETTE
barplot(x, col = topo.colors(6))

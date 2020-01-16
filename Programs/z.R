#!/usr/bin/env Rscript
pdf("xh.pdf") # set graphical output file
hist(rnorm(100)) # generate 100 N(0, 1) variates and plot their histogram
graphics.off() # close the graphical output file

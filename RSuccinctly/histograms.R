#!/usr/bin/env Rscript
# LOAD DATA SET
library("datasets")
# Annual Canadian Lynx trappings 1821-1934
data(lynx)
# HISTOGRAM WITH DEFAULTS
hist(lynx)
# HISTOGRAM WITH OPTIONS
hist(lynx,
  breaks = 14, # "Suggests" 14 bins.
  freq = FALSE, # Axis shows density, not frequency.
  col = "thistle1", # Color for the histogram.
  main = "Histogram of Annual Canadian Lynx Trappings\n1821-1934",
  xlab = "Number of Lynx Trapped"
) # Label X axis

# SUPERIMPOSED NORMAL DISTRIBUTION
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)), # Shape, mean, SD
  col = "thistle4", # Color of the curve.
  lwd = 2, # Line width of 2 pixels.
  add = TRUE
) # Superimpose on the previous graph.
# SUPERIMPOSED KERNEL DENSITY ESTIMATES
lines(density(lynx), col = "blue", lwd = 2)
lines(density(lynx, adjust = 3), col = "darkgreen", lwd = 2)
# SUPERIMPOSED RUG PLOT
rug(lynx, lwd = 2)
# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

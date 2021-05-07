#!/usr/bin/env Rscript
# Sample: sample_2_5.R
# LOAD DATA SET
# Load datasets package.
library("datasets")
# Annual Canadian Lynx trappings 1821-1934.
data(lynx)
# BOXPLOT WITH DEFAULTS
boxplot(lynx)
# BOXPLOT WITH OPTIONS
boxplot(lynx,
  horizontal = TRUE, # Draw boxplot horizontally.
  las = 1, # Make all labels horizontal.
  notch = TRUE, # Notches for CI for median.
  col = "slategray3", # Color for the central box.
  boxwex = 0.5, # Width of box as proportion of original.
  whisklty = 1, # Whisker line type; 1 = solid line
  staplelty = 0, # Staple (line at end) type; 0 = none
  outpch = 16, # Symbols for outliers; 16 = filled circle
  outcol = "slategray3", # Color for outliers.
  main = "Boxplot of Annual Canadian Lynx Trappings\n1821-1934",
  xlab = "Number of Lynx Trapped"
) # Label the x-axis.
# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Removes all objects from workspace.
rm(list = ls())

#!/usr/bin/env Rscript
# LOAD DATA
# Load datasets package
require("datasets")
# Load data into workspace
data(warpbreaks)
# RESTRUCTURE DATA
wbdata <- tapply(warpbreaks$breaks, # Outcome
  list(warpbreaks$wool, warpbreaks$tension), # Factors
  FUN = mean
) # Summary function
# CREATE BARPLOT
barplot(wbdata, # Use a new summary table.
  beside = TRUE, # Bars side-by-side vs. stacked
  col = c("steelblue3", "thistle3"), # Colors
  main = "Mean Number of Warp Breaks\nby Tension and Wool",
  xlab = "Tension",
  ylab = "Mean Number of Breaks"
)
# ADD LEGEND
legend("topright",
  rownames(wbdata), # Use matrix row names (A & B)
  fill = c("steelblue3", "thistle3")
) # Colors

#!/usr/bin/env Rscript
# LOAD DATA
# Use data set "painters" from the package "MASS"
library("MASS")
library("RColorBrewer")
data(painters)
painters[1:3, ]
# GROUPED BOXPLOTS WITH DEFAULTS
# Draw boxplots of outcome (Expression) by group (School)
boxplot(painters$Expression ~ painters$School)

# GROUPED BOXPLOTS WITH OPTIONS
boxplot(painters$Expression ~ painters$School,
  col = brewer.pal(8, "Pastel2"),
  names = c(
    "Renais.",
    "Mannerist",
    "Seicento",
    "Venetian",
    "Lombard",
    "16th C.",
    "17th C.",
    "French"
  ),
  boxwex = 0.5, # Width of box as proportion of original.
  whisklty = 1, # Whisker line type; 1 = solid line
  staplelty = 0, # Staple (line at end) type; 0 = none.
  outpch = 16, # Symbols for outliers; 16 = filled circle.
  outcol = brewer.pal(8, "Pastel2"), # Color for outliers.
  main = "Expression Ratings of Painters by School
 from \"painters\" Data set in \"MASS\" Package",
  xlab = "Painter's School",
  ylab = "Expression Ratings"
)
# CLEAN UP
# Unloads MASS package
detach("package:MASS", unload = TRUE)
# Unloads RColorBrewer
detach("package:RColorBrewer", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

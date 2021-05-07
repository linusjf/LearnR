#!/usr/bin/env Rscript
# ENTER DATA 
data1 <- as.matrix(read.table( # Save as matrix
 header = TRUE, # First row is the header.
 row.names = 1, # First column is row names.
 # No comments within the data.
 text = '
 X A B C
 L 5 3 1
 R 2 4 6
 '))
# Check the data.
data1

# CREATE BARPLOT
barplot(data1, # Use a new summary table.
 beside = TRUE, # Bars side-by-side vs. stacked.
 col = c("steelblue3", "thistle3"), # Colors
 main = "Side by Side Barplots",
 xlab = "Groups",
 ylab = "frequency")

# ADD LEGEND INTERACTIVELY
legend(5,5.5, # Use mouse to locate the legend.
 rownames(data1), # Use matrix row names (A & B).
 fill = c("steelblue3", "thistle3")) # Colors

rm(list = ls())

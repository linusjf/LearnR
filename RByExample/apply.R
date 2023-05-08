#!/usr/bin/env Rscript

# Create data frame
example <- data.frame(
  indiv = c("A", "B", "C", "D", "E"),
  height_0 = c(15, 10, 12, 9, 17),
  height_10 = c(20, 18, 14, 15, 19),
  height_20 = c(23, 24, 18, 17, 26)
)

# View the data frame
head(example)

# Calculating the mean for each row in the data frame
row.avg <- apply(X = example[, 2:4], MARGIN = 1, FUN = mean)

# View row.avg
row.avg

# Calculating the mean for each column in the data frame
col.avg <- apply(example[, 2:4], 2, mean)

# View col.avg
col.avg

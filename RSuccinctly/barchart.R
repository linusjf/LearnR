#!/usr/bin/env Rscript
# LOAD DATA
# Load the datasets package.
library("datasets")
# Load data with shorter name.
spray <- InsectSprays
# GET GROUP MEANS
means <- aggregate(spray$count ~ spray$spray, FUN = mean)
# Check the data.
means
# REORGANIZE DATA FOR BARPLOT
mean.data <- t(means[-1]) # Removes the first column, transposes the second.
colnames(mean.data) <- means[, 1] # Add group names as column names.
mean.data
# BARPLOT WITH DEFAULTS
barplot(mean.data)

# BARPLOT WITH OPTIONS
barplot(mean.data,
  col = "red",
  main = "Effectiveness of Insect Sprays",
  xlab = "Spray Used",
  ylab = "Insect Count"
)
# CLEAN UP
# Unloads the datasets package.
detach("package:datasets", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

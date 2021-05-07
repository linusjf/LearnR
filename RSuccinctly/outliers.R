#!/usr/bin/env Rscript
# LOAD DATA & INITIAL CHECKS
library("datasets")
# Areas (in 1k sq mi) of landmasses > 10k sq mi (n = 48)
data(islands)
# Many high outliers.
boxplot(islands, horizontal = TRUE)
# Numbers for the boxplot.
boxplot.stats(islands)

# DELETE OUTLIERS
# Delete 8 highest
islands.low <- islands[islands < 500]
boxplot(islands.low, horizontal = TRUE)
boxplot.stats(islands.low)
# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Remove all objects from the workspace.
rm(list = ls())

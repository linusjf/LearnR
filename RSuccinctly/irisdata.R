#!/usr/bin/env Rscript
# LOAD DATA
require("datasets")
# Load the data sets package.
# PREVIEW DATA
# Show first three rows, all variables.
iris[1:3, ]
# COMPARE GROUPS ON ONE VARIABLE
aggregate(iris$Petal.Width ~ iris$Species,
  FUN = mean
)
# COMPARE GROUPS ON TWO VARIABLES
aggregate(cbind(
  iris$Petal.Width,
  iris$Petal.Length
)
~ iris$Species,
FUN = mean
)
# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

#!/usr/bin/env Rscript
# LOAD DATA
library("datasets")
# LOAD "CAR" PACKAGE
suppressMessages(library("car"))
# "Companion to Applied Regression"
# Load the datasets package.
# Load data into workspace.
data(iris)
# Show the first three lines of data.
iris[1:3, ]

# SCATTERPLOT BY GROUPS
# Group by species.
scatterplot(Sepal.Width ~ Sepal.Length | Species,
  data = iris,
  xlab = "Sepal Width",
  ylab = "Sepal Length",
  main = "Iris Data",
  smooth = TRUE,
  grid = TRUE,
  frame = TRUE,
  ellipse = TRUE,
  boxplots = "xy",
  legend = c(coords = "topright")
)
# CLEAN UP
# Unloads the datasets package.
detach("package:datasets", unload = TRUE)
# Unloads the car package.
detach("package:car", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

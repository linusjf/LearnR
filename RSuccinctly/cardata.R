#!/usr/bin/env Rscript
# LOAD DATA
library("datasets")
# Load datasets package.
# 1974 road test data from Motor Trend.
data(mtcars)
# Show all variables for the first three cars.
mtcars[1:3, ]
# ALL CASES
# Mean horsepower for all cars.
mean(mtcars$hp)

# SELECT ON SINGLE VARIABLE
# Mean horsepower (for 8-cylinder cars).
# Select rows where cyl = 8
mean(mtcars$hp[mtcars$cyl == 8])
# CREATE NEW DATA FRAME WITH SELECTION
# 8-cylinder cars, all variables
v8 <- mtcars[mtcars$cyl == 8, ]
# SELECT ON TWO VARIABLES
# Mean horsepower for cars with v8, 5-speed, and weigh < 4000 lbs.
# Show the mean horsepower.
mean(v8$hp[v8$gear == 5 & v8$wt < 4])
# List the cars included.
v8[v8$gear == 5 & v8$wt < 4, c(2, 10, 6, 4)]
# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

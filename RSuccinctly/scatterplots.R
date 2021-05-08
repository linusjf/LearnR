#!/usr/bin/env Rscript
# LOAD DATA
# Load datasets package.
library("datasets")
library("car")
data(cars)
# SCATTER PLOT WITH DEFAULTS
plot(cars)

# SCATTER PLOT WITH OPTIONS
plot(cars,
  pch = 16,
  col = "red",
  main = "Speed vs. Stopping Distance for Cars in
 1920s from \"cars\" Data set",
  xlab = "Speed (MPH)",
  ylab = "Stopping Distance (feet)"
)

# ADD REGRESSION & LOWESS
# Linear regression line.
abline(lm(cars$dist ~ cars$speed), col = "red")
# "locally weighted scatterplot smoothing"
lines(lowess(cars$speed, cars$dist), col = "blue")

# "CAR" SCATTERPLOT
sp(cars$dist ~ cars$speed, # Distance as a function of speed.
  pch = 16, # Points: solid circles.
  col = "red", # Red for graphic elements.
  main = "Speed vs. Stopping Distance for Cars in
 1920s from \"cars\" Data set",
  xlab = "Speed (MPH)",
  ylab = "Stopping Distance (feet)"
)

# CLEAN UP
# Unloads datasets package.
detach("package:datasets", unload = TRUE)
# Unloads car package.
detach("package:car", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

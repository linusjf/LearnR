#!/usr/bin/env Rscript
# LOAD DATA SET
# Load the data sets package
library("datasets")
library("psych")
# Print the cars data structure to the console
str(cars)
# Load the data into the workspace
data(cars)
# Summary for one variable
summary(cars$speed)
# Summary for entire table (inc. categories)
summary(cars)
# ALTERNATIVE DESCRIPTIVES
describe(cars)
# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Unloads psych package.
detach("package:psych", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

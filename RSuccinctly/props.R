#!/usr/bin/env Rscript
# CREATE SIMULATION DATA
# Two vectors are needed:
# One specifies the total number of people in each group.
# This creates a vector with 5 100s in it, for 5 groups.
n5 <- c(rep(100, 5))
# Another specifies the number of cases with positive outcomes.
x5 <- c(65, 60, 60, 50, 45)
# PROPORTION TEST
prop.test(x5, n5)
# CREATE SIMULATION DATA FOR 2 GROUPS
# 40 trials
n2 <- c(40, 40) 
# Number of positive outcomes
x2 <- c(30, 20) 
# PROPORTION TEST FOR 2 GROUPS
# With CI
prop.test(x2, n2, conf.level = .80)

# CLEAN UP
# Remove all objects from the workspace.
rm(list = ls()) 

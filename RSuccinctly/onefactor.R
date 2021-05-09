#!/usr/bin/env Rscript
# CREATE SIMULATION DATA
# Step 1: Each group in a separate variable.
# Group 1, mean = 40
x1 <- rnorm(60, mean = 40, sd = 8)
# Group 2, mean = 41
x2 <- rnorm(60, mean = 41, sd = 8)
# Group 3, mean = 44
x3 <- rnorm(60, mean = 44, sd = 8)
# Group 4, mean = 45
x4 <- rnorm(60, mean = 45, sd = 8)
# Step 2: Combine vectors into a single data frame.
xdf <- data.frame(cbind(x1, x2, x3, x4))
# Step 3: Stack data to get the outcome column and group column.
xs <- stack(xdf)

# ONE-FACTOR ANOVA
# Basic model.
anova1 <- aov(values ~ ind, data = xs)
# Model summary.
summary(anova1)

# POST-HOC COMPARISONS
TukeyHSD(anova1)

# CLEAN UP
# Remove all objects from workspace
rm(list = ls())

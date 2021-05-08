#!/usr/bin/env Rscript
# LOAD DATA & SELECT VARIABLES
library("datasets")
# Load the datasets package.
sleep[1:5, ]
# Show the first 5 cases.
# Save just the first two variables.
sd <- sleep[, 1:2]
sd[1:5, ]
# Show the first 5 cases.
# GRAPHICAL CHECKS
hist(sd$extra)
# Histogram of extra sleep.
# Boxplots by group.
boxplot(extra ~ group, data = sd)
# TWO-SAMPLE T-TEST WITH DEFAULTS
t.test(extra ~ group, data = sd)

# TWO-SAMPLE T-TEST WITH OPTIONS
t.test(extra ~ group, # Same: Specifies variables.
  data = sd, # Same: Specifies data set.
  alternative = "less", # One-tailed test.
  conf.level = 0.80
) # 80% CI (vs. 95%)

# SIMULATED DATA IN TWO VARIABLES
x <- rnorm(30, mean = 20, sd = 5)
y <- rnorm(30, mean = 24, sd = 6)
t.test(x, y)

# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

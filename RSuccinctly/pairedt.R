#!/usr/bin/env Rscript
# CREATE SIMULATED DATA
t1 <- rnorm(50, mean = 52, sd = 6) # Time 1
dif <- rnorm(50, mean = 6, sd = 12) # Difference
# Time 2
t2 <- t1 + dif
# PAIRED T-TEST WITH DEFAULTS
# Must specify "paired"
t.test(t2, t1, paired = TRUE)

# PAIRED T-TEST WITH OPTIONS
t.test(t2, t1,
  paired = TRUE,
  mu = 6, # Specify a non-0 null value.
  alternative = "greater", # One-tailed test
  conf.level = 0.99
) # 99% CI (vs. 95%)
# CLEAN UP
# Remove all objects from the workspace.
rm(list = ls())

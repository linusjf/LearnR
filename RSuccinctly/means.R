#!/usr/bin/env Rscript
# LOAD DATA & EXAMINE
require("datasets")
# Loads data sets package.
# Loads just the magnitude variable.
mag <- quakes$mag
hist(mag)
summary(mag)

# T-TEST WITH DEFAULTS
t.test(mag)

# T-TEST WITH OPTIONS
t.test(mag,
  alternative = "greater", # Directional test
  mu = 4.5, # Null population mean of 4.5
  conf.level = 0.99
) # Confidence level of 99%

# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())

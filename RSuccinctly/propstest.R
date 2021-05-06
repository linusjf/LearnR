#!/usr/bin/env Rscript
# PROPORTIONS TEST WITH DEFAULTS
# 27 heads in 40 coin flips.
prop.test(27, 40)

# PROPORTION TEST WITH OPTIONS
prop.test(27, 40, # Same observed values.
  p = .6, # Null probability of .6 (vs. .5).
  alt = "greater", # Directional test for greater value.
  conf.level = .90
) # Confidence level of 90% (vs. 95%).

rm(list = ls())

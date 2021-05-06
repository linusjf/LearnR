#!/usr/bin/env Rscript
require(graphics)
pairs(quakes,
  main = "Fiji Earthquakes, N = 1000",
  cex.main = 1.2,
  pch = "."
)

#!/usr/bin/env Rscript
require(graphics)
## Full mosaic
mosaicplot(HairEyeColor)
## Aggregate over sex (as in Snee's original data)
x <- apply(
  HairEyeColor,
  c(1, 2),
  sum
)
print(x)
mosaicplot(x,
  main = "Relation between hair and eye color"
)

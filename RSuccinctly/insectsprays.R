#!/usr/bin/env Rscript
require(stats)
require(graphics)
boxplot(count ~ spray,
  data = InsectSprays,
  xlab = "Type of spray",
  ylab = "Insect count",
  main = "InsectSprays data",
  varwidth = TRUE,
  col = "lightgray"
)
fm1 <- aov(count ~ spray, data = InsectSprays)
summary(fm1)
opar <- par(
  mfrow = c(2, 2),
  oma = c(0, 0, 1.1, 0)
)
plot(fm1)
fm2 <- aov(sqrt(count) ~ spray, data = InsectSprays)
summary(fm2)
plot(fm2)
par(opar)
rm(list = ls())

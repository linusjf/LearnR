#!/usr/bin/env Rscript
library(stats)
library(graphics)
pairs(swiss,
  panel = panel.smooth, main = "swiss data",
  col = 3 + (swiss$Catholic > 50)
)
summary(lm(Fertility ~ ., data = swiss))

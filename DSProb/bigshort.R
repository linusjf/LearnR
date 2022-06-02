#!/usr/bin/env Rscript
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
print("Interest on loan:")
x <- r * 180000
print("Expected value per loan:")
loss_per_foreclosure * p + x * (1-p)

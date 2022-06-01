#!/usr/bin/env Rscript

print("What is the probability of guessing correctly for one question?")
p <- 0.2
p
a <- 1
b <- -0.25
mu <- a * p + b * (1 -p)
mu



#!/usr/bin/env Rscript

print("What is the probability of guessing correctly for one question?")
p <- 0.2
p
print("What is the expected value of points for guessing on one question?")
a <- 1
b <- -0.25
mu <- a * p + b * (1 -p)
mu
print("What is the expected score of guessing on all 44 questions?")
n <- 44
n * mu



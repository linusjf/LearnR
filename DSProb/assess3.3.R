#!/usr/bin/env Rscript

print("What is the probability of guessing correctly for one question?")
p <- 0.2
q <- 1 - p
print("What is the expected value of points for guessing on one question?")
a <- 1
b <- -0.25
mu <- a * p + b * q
mu
print("What is the expected score of guessing on all 44 questions?")
n <- 44
n * mu
print("What is the standard error of guessing on all 44 questions?")
sigma <- abs(b - a) * sqrt(p * q)
se <- sqrt(n) * sigma
se

#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(tidyverse))
set.seed(1)
print("Create a random variable S with the earnings of your bank if you give out 10,000 loans, the default rate is 0.3, and you lose $200,000 in each foreclosure.")
n <- 10000
loss_per_foreclosure <- -200000
# probability of default
p <- 0.3
loan.amt <- 180000
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
S <- sum(defaults * loss_per_foreclosure)
S

print("Run a Monte Carlo simulation with 10,000 outcomes for S. Make a histogram of the results.")

print("What is the expected value of S?")

print("What is the standard error of S?")

print("Suppose we give out loans for $180,000. What should the interest rate be so that our expected value is 0?")

print("(Harder) What should the interest rate be so that the chance of losing money is 1 in 20? In math notation, what should the interest rate be so that Pr( S < 0 ) =0.05")

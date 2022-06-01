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
print("Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.")
1 - pnorm(8, mu, se)

print("Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.")
print("What is the probability that a guessing student scores 8 points or higher?")
## Warning in set.seed(21, sample.kind = "Rounding"): non-uniform 'Rounding'
defaultW <- getOption("warn")
options(warn = -1)
set.seed(21, sample.kind = "Rounding")
options(warn = defaultW)
## sampler used
B <- 10000
S <- replicate(B, {
  Results <- sample(c(a, b), n, replace = TRUE, prob = c(p, q))
  sum(Results)
})
mean(S > 8)

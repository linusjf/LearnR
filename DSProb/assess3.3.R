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

print("Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.")
print("What is the expected value of the score when guessing on this new test?")
p <- 1 / 4
q <- 1 - p
a <- 1
b <- 0
n <- 44
mu <- n * (a * p + b * q)
mu

print("Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.")
print("What is the lowest p such that the probability of scoring over 35 exceeds 80%?")
p <- seq(0.25, 0.95, 0.05)

prob <- function(p) {
  e_points <- (a * p) + (b * (1 - p))
  m <- n * e_points
  se <- sqrt(n) * abs(a - b) * sqrt(p * (1 - p))
  1 - pnorm(35, m, se)
}

score <- sapply(p, prob)
min(p[which(score > 0.8)])

print("A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.")
print("What is the expected value of the payout for one bet?")
p <- 5 / 38
a <- 6
b <- -1
mu <- a * p + b * (1 - p)
mu

n <- 500

print("What is the standard error of the payout for one bet?")
sigma <- abs(b - a) * sqrt(p * (1 - p))
sigma

print("What is the expected value of the average payout over 500 bets?")
mu

print("What is the standard error of the average payout over 500 bets?")
sigma / sqrt(n)

print("What is the expected value of the sum of 500 bets?")
n * mu

print("What is the standard error of the sum of 500 bets?")
sqrt(n) * sigma

print("Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets?")
pnorm(0, n * mu, sqrt(n) * sigma)

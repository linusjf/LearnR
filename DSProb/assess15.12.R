#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(tidyverse))
print("Create a random variable S with the earnings of your bank if you give out 10,000 loans, the default rate is 0.3, and you lose $200,000 in each foreclosure.")
n <- 10000
loss_per_foreclosure <- -200000
# probability of default
p <- 0.3
loan.amt <- 180000
defaults <- sample(c(0, 1), n, prob = c(1 - p, p), replace = TRUE)
S <- sum(defaults * loss_per_foreclosure)
S / 10^6

print("Run a Monte Carlo simulation with 10,000 outcomes for S. Make a histogram of the results.")
B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0, 1), n, prob = c(1 - p, p), replace = TRUE)
  sum(defaults * loss_per_foreclosure)
})
data.frame(losses_in_millions = losses / 10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(color = "black", binwidth = 5)

print("What is the expected value of S (in millions)?")
mu <- n * (loss_per_foreclosure * p + 0 * (1 - p))
mu / 10^6

print("What is the standard error of S (in millions)?")
se <- sqrt(n * p * (1 - p)) * abs(loss_per_foreclosure)
se / 10^6

print("Suppose we give out loans for $180,000. What should the interest rate be so that our expected value is 0?")
x <- -loss_per_foreclosure * p / (1 - p)
x
x / loan.amt

print("(Harder) What should the interest rate be so that the chance of losing money is 1 in 20? In math notation, what should the interest rate be so that Pr( S < 0 ) = 0.05")
z <- qnorm(0.05)
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l * (n * p - z * sqrt(n * p * (1 - p))) / (n * (1 - p) + z * sqrt(n * p * (1 - p)))
x
x / loan.amt

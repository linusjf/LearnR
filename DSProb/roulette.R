#!/usr/bin/env Rscript
# sampling model 1: define urn, then sample
# define the urn for the sampling model
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
# 1000 independent draws
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9 / 19, 10 / 19))
# total winnings = sum of draws
S <- sum(x)
print("Total winnings for casino: ")
S

# number of roulette players
n <- 1000
# number of Monte Carlo experiments
B <- 10000
S <- replicate(B, {
  # simulate 1000 spins
  X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9 / 19, 10 / 19))
  # determine total profit
  sum(X)
})

print("probability of the casino losing money")
mean(S < 0)

suppressPackageStartupMessages(library(tidyverse))
# sequence of 100 values across range of S
s <- seq(min(S), max(S), length = 100)
# generate normal density for S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S)))
# make data frame of S for histogram
data.frame(S = S) %>%
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

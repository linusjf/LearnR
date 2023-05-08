#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(tidyverse))
library(dslabs)

set.seed(16,
  sample.kind = "Rounding"
)
act.mean <- 20.9
act.sd <- 5.7
n <- 10000
act.scores <- rnorm(n, act.mean, act.sd)

print("mean of act.scores: ")
mean(act.scores)

print("sd of act.scores: ")
sd(act.scores)

# plot distribution of simulated ACT scores
data.frame(act.scores = act.scores) %>%
  ggplot(aes(act.scores)) +
  geom_histogram(color = "black", binwidth = 2)

print("How many perfect scores (= 36)?")
sum(act.scores >= 36)

print("In act_scores, what is the probability of an ACT score greater than 30?")
sum(act.scores > 30) / n

print("In act_scores, what is the probability of an ACT score less than or equal to 10?")
sum(act.scores <= 10) / n

x <- seq(1:36)
fx <- dnorm(x, 20.9, 5.7)
data.frame(x, fx) %>%
  ggplot(aes(x, fx)) +
  geom_line()

zscores <-
  (act.scores - mean(act.scores)) / sd(act.scores)
print("What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?")
sum(zscores > 2) / n

print("What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?")
2 * sd(act.scores) + mean(act.scores)

print("What is the 97.5th percentile of act_scores?")
qnorm(0.975, mean(act.scores), sd(act.scores))

F <- function(a) mean(act.scores <= a)

x <- seq(1, 32)

cdf <- sapply(x, F)

print("What is the minimum integer score such that the probability of that score or lower is at least .95?")

min(which(cdf >= 0.95))

print("Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.")
print("What is the expected 95th percentile of ACT scores?")

qnorm(0.95, 20.9, 5.7)

print("Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles.")
print("In what percentile is a score of 26?")

p <- seq(0.01, 0.99, 0.01)

sample.quantiles <- quantile(act.scores, p)

names(sample.quantiles[max(which(sample.quantiles < 26))])

theoretical.quantiles <- qnorm(p, 20.9, 5.7)

qplot(theoretical.quantiles, sample.quantiles) + geom_abline()

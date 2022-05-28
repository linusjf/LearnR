#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(tidyverse))
library(dslabs)

set.seed(16, 
         sample.kind = "Rounding")
act.mean <- 20.9
act.sd <- 5.7
n <- 10000
act.scores <- rnorm(n,act.mean,act.sd)

print("mean of act.scores: ")
mean(act.scores)

print("sd of act.scores: ")
sd(act.scores)

# plot distribution of simulated ACT scores
data.frame(act.scores = act.scores) %>%
    ggplot(aes(act.scores)) +
    geom_histogram(color="black", binwidth = 2)

print("How many perfect scores (= 36)?")
sum(act.scores >= 36)

print("In act_scores, what is the probability of an ACT score greater than 30?")
sum(act.scores > 30)/n

print("In act_scores, what is the probability of an ACT score less than or equal to 10?")
sum(act.scores <= 10)/n

x <- seq(1:36)
fx <- dnorm(x,20.9,5.7)
data.frame(x, fx) %>%
    ggplot(aes(x,fx)) +
    geom_line()

zscores <- 
  (act.scores - mean(act.scores))/sd(act.scores)
print("What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?")
sum(zscores > 2)/n

print("What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?")
2 * sd(act.scores) + mean(act.scores)

print("What is the 97.5th percentile of act_scores?")
qnorm(0.975,mean(act.scores),sd(act.scores))

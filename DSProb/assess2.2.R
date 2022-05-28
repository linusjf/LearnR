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

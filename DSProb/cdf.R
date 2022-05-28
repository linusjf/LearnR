#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(tidyverse))
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

F <- function(a) mean(x <= a)

print("probability of male taller than 70")
1 - F(70)   

print("probability that a male is taller than 70.5 inches using pnorm")
1 - pnorm(70.5, mean(x), sd(x))

print("Discretization and the normal approximation")
print("plot distribution of exact heights in data")
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

print("probabilities in actual data over length 1 ranges containing an integer")
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

print("probabilities in normal approximation match well")
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

print("probabilities in actual data over other ranges don't match normal approx as well")
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1,sd(x))

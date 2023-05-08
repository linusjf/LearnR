#!/usr/bin/env Rscript
# defines vector of many B values
B <- 10^seq(1, 5, len = 100)
print(B)

# function to run Monte Carlo simulation with each B
compute_prob <- function(B, n = 22) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

# apply compute_prob to many values of B
prob <- sapply(B, compute_prob)
plot(log10(B), prob, type = "l")

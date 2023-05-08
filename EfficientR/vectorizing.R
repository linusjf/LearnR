#!/usr/bin/env Rscript
library(microbenchmark)

method1 <- function(n) {
  vec <- NULL
  for (i in seq_len(n)) {
    vec <- c(vec, i)
  }
  return(vec)
}

method2 <- function(n) {
  vec <- numeric(n)
  for (i in seq_len(n)) {
    vec[i] <- i
  }
  return(vec)
}

method3 <- function(n) seq_len(n)

log_sum_for_loop <- function(x) {
  log_sum <- 0
  for (i in seq_along(x)) {
    log_sum <- log_sum + log(x[i])
  }
  return(log_sum)
}

log_sum <- function(x) sum(log(x))

monte_carlo <- function(n) {
  hits <- 0
  for (i in seq_len(n)) {
    u1 <- runif(1)
    u2 <- runif(1)
    if (u1^2 > u2) {
      hits <- hits + 1
    }
  }
  return(hits / n)
}

monte_carlo_vec <- function(n) {
  sum(runif(n)^2 > runif(n)) / n
}

main <- function(argv) {
  n <- 10000
  print(microbenchmark(times = 100, unit = "s", method1(n), method2(n), method3(n)))
  x <- runif(n)
  print(microbenchmark(times = 100, unit = "s", log_sum_for_loop(x), log_sum(x)))
  x <- numeric()
  print(microbenchmark(times = 100, unit = "s", log_sum_for_loop(x), log_sum(x)))
  n <- 5e+05
  print(system.time(monte_carlo(n)))
  print(system.time(monte_carlo_vec(n)))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

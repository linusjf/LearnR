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

main <- function(argv) {
  n <- 1e+4
  print(microbenchmark(
    times = 100, unit = "s",
    method1(n), method2(n), method3(n)
  ))
  x <- runif(n)
  print(microbenchmark(
    times = 100, unit = "s",
    log_sum_for_loop(x), log_sum(x)
  ))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

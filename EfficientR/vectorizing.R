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

main <- function(argv) {
  n <- 1e+4
  print(microbenchmark(
    times = 100, unit = "s",
    method1(n), method2(n), method3(n)
  ))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

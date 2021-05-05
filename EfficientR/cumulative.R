#!/usr/bin/env Rscript
library(microbenchmark)

# Method 1: with a for loop (10 lines)
cs_for <- function(x) {
  for (i in x) {
    if (i == 1) {
      xc <- x[i]
    } else {
      xc <- c(xc, sum(x[1:i]))
    }
  }
  return(xc)
}

# Method 2: with apply (3 lines)
cs_apply <- function(x) {
  sapply(x, function(x) sum(1:x))
}

main <- function(argv) {
  # initiate vector to cumulatively sum
  x <- 1:100

  # Method 3: cumsum (1 line, not shown)
  print(microbenchmark(cs_for(x), cs_apply(x), cumsum(x)))
  x <- 1:50000
  print(microbenchmark(cs_for(x), cs_apply(x), cumsum(x), times = 1, unit = "s"))

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

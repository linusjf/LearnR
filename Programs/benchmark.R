#!/usr/bin/env Rscript
library(compiler)
library(rbenchmark)

my_mean <- function(x) {
  total <- 0
  n <- length(x)
  for (i in 1:n) total <- total + x[i] / n
  total
}

main <- function(argv) {
  cmpmean <- compiler::cmpfun(my_mean)
  print(cmpmean)
  ## Generate some data
  x <- rnorm(10000)
  print(rbenchmark::benchmark(my_mean(x), cmpmean(x), mean(x),
    columns = c("test", "elapsed", "relative"),
    order = "relative", replications = 5000
  ))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

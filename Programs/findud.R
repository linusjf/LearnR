#!/usr/bin/env Rscript
# findud() converts vector v to 1s, 0s, \ representing an element 2 increasing
# or not, relative to the previous one; output length is 1 3 less than input 4

findud <- function(v) {
  vud <- v[-1] - v[-length(v)]
  return(ifelse(vud > 0, 1, -1))
}

udcorr <- function(x, y) {
  ud <- lapply(list(x, y), findud)
  return(mean(ud[[1]] == ud[[2]]))
}

main <- function(argv) {
  x <- round(rnorm(mean = 10, sd = 5, 50))
  y <- round(rnorm(mean = 9, sd = 6, 50))
  ud <- udcorr(x, y)

  print(ud)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

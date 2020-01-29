#!/usr/bin/env Rscript

vector_sum_loop <- function(v) {
  result <- 0
  for (i in v) {
    result <- result + i
  }
  return(result)
}

vector_sum_rec <- function(v) {
  if (length(v) == 1) {
    return(v)
  } else {
    return(v[1] + vector_sum_rec(v[-1]))
  }
}


main <- function(argv) {
  print(vector_sum_loop(c(5, 40, 91)))
  print(vector_sum_rec(c(5, 40, 91)))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

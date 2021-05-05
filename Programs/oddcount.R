#!/usr/bin/env Rscript
# counts the number of odd integers in x
oddcount <- function(x) {
  k <- 0  # assign 0 to k
  for (n in x) {
    # %% is the modulo operator
    if (n%%2 == 1) {
      k <- k + 1
    }
  }
  return(k)
}

main <- function(argv) {
  print(oddcount(c(1, 3, 5)))
  print(oddcount(c(1, 2, 3, 7, 9)))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

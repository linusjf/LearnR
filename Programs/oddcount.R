#!/usr/bin/env Rscript
# counts the number of odd integers in x
oddcount <- function(x) {
  k <- 0  # assign 0 to k 
  for (n in x) {
    if (n%%2 == 1) 
      k <- k + 1
    # %% is the modulo operator
  }
  return(k)
}

oddcount(c(1, 3, 5))
oddcount(c(1, 2, 3, 7, 9))

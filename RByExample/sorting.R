#!/usr/bin/env Rscript
# Goal: To do sorting.  The approach here needs to be explained. If `i' is a
# vector of integers, then the data frame D[i,] picks up rows from D based on the
# values found in `i'.  The order() function makes an integer vector which is a
# correct ordering for the purpose of sorting.
main <- function(argv) {
  D <- data.frame(x = c(1, 2, 3, 1), y = c(7, 19, 2, 2))
  print(D)

  # Sort on x
  indexes <- order(D$x)
  print(D[indexes, ])
  indexes <- order(-D$x)
  print(D[indexes, ])

  # Print out sorted dataset, sorted in reverse by y
  print(D[order(D$y), ])
  print(D[order(-D$y), ])
  print(D[order(D$y), ])
  print(D[rev(order(D$y)), ])
  return(0)
}

if (identical(environment(), globalenv())) quit(status = main(commandArgs(trailingOnly = TRUE)))

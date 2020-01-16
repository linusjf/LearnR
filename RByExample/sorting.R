#!/usr/bin/env Rscript
# Goal: To do sorting.  The approach here needs to be explained. If `i' is a
# vector of integers, then the data
# frame d[i,] picks up rows from d based on the
# values found in `i'.  The order() function makes an integer vector which is a
# correct ordering for the purpose of sorting.
main <- function(argv) {
  d <- data.frame(x = c(1, 2, 3, 1), y = c(7, 19, 2, 2))
  print(d)

  # Sort on x
  indexes <- order(d$x)
  print(d[indexes, ])
  indexes <- order(-d$x)
  print(d[indexes, ])

  # Print out sorted dataset, sorted in reverse by y
  print(d[order(d$y), ])
  print(d[order(-d$y), ])
  print(d[order(d$y), ])
  print(d[rev(order(d$y)), ])
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

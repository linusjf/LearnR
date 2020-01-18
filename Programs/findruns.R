#!/usr/bin/env Rscript
findruns <- function(x, k) {
  n <- length(x)
  runs <- vector(length = n)
  count <- 0
  for (i in 1:(n - k + 1)) {
    if (all(x[i:(i + k - 1)] == 1)) {
      count <- count + 1
      runs[count] <- i
    }
  }
  if (count > 0) {
    runs <- runs[1:count]
  } else {
    runs <- NULL
  }
  return(runs)
}

main <- function(argv) {
  y <- c(1, 0, 0, 1, 1, 1, 0, 1, 1)
  a <- findruns(y, 2)
  print(a)
  b <- findruns(y, 3)
  print(b)
  c <- findruns(y, 6)
  print(c)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

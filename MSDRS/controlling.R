#!/usr/bin/env Rscript

# nolint start
main <- function(argv) {
  ## Generate a uniform random number
  x <- runif(1, 0, 10)
  if (x > 3) {
    y <- 10
  } else {
    y <- 0
  }
  print(y)

  numbers <- rnorm(10)
  for (i in 1:10) {
    print(numbers[i])
  }

  x <- c("a", "b", "c", "d")

  for (i in 1:4) {
    ## Print out each element of 'x'
    print(x[i])
  }

  ## Generate a sequence based on length of 'x'
  for (i in seq_along(x)) {
    print(x[i])
  }

  for (letter in x) {
    print(letter)
  }

  for (i in 1:4) print(x[i])

  x <- matrix(1:6, 2, 3)

  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      print(x[i, j])
    }
  }

  for (i in 1:100) {
    if (i <= 20) {
      ## Skip the first 20 iterations
      next
    }
    ## Do something here
    print(i)
  }

  for (i in 1:100) {
    print(i)

    if (i > 20) {
      ## Stop loop after 20 iterations
      break
    }
  }
  return(0)
}
# nolint end

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

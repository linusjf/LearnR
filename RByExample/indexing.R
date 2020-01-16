#!/usr/bin/env Rscript
# Goal: To show amazing R indexing notation, and the use of is.na()

# An example vector to play with.
main <- function(argv) {
  x <- c(2, 7, 9, 2, NA, 5)
  cat("This is the starting vector x:\n")
  print(x)
  # Give me elems 1 to 3 --
  cat("print(x[1:3])\n")
  print(x[1:3])
  cat("print(x[-1])\n")
  print(x[-1])
  # Odd numbered elements --
  cat("indexes <- seq(1,6,2)\n")
  indexes <- seq(1, 6, 2)
  cat("print(x[indexes])\n")
  print(x[indexes])
  # or, more compactly,
  cat("print(x[seq(1,6,2)])\n")
  print(x[seq(1, 6, 2)])
  # Access elements by specifying 'on' / 'off' through booleans --
  cat("require <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)\n
  ")
  require <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  cat("print(x[require])\n")
  print(x[require])
  # Short vectors get reused! So, to get odd numbered elems --
  cat("print(x[c(TRUE,FALSE)])\n")
  print(x[c(TRUE, FALSE)])
  # Locate missing data --
  cat("print(is.na(x))\n")
  print(is.na(x))
  cat("x[is.na(x)] <- 0\n")
  x[is.na(x)] <- 0
  print(x)
  # Similar ideas work for matrices --
  cat("y <- matrix(c(2,7,9,2,NA,5), nrow=2)\n")
  y <- matrix(c(2, 7, 9, 2, NA, 5), nrow = 2)
  print(y)
  # Make a matrix containing columns 1 and 3 --
  cat("print(y[,c(1,3)])\n")
  print(y[, c(1, 3)])
  # Let us see what is.na(y) does --
  cat("print(is.na(y))\n")
  print(is.na(y))
  cat("str(is.na(y))\n")
  str(is.na(y))
  # Hence I can say
  cat("y[is.na(y)] <- -1\n")
  y[is.na(y)] <- -1
  print(y)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

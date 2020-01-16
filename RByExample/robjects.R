#!/usr/bin/env Rscript
# Goals: A first look at R objects - vectors, lists, matrices, data frames.

# To make vectors 'x' 'y' 'year' and 'names'
main <- function(argv) {
  x <- c(2, 3, 7, 9)
  y <- c(9, 7, 3, 2)
  year <- 1990:1993
  names <- c("payal", "shraddha", "kritika", "itida")
  # Accessing the 1st and last elements of y --
  print(y[1])
  print(y[length(y)])

  # To make a list 'person' --
  person <- list(name = "payal", x = 2, y = 9, year = 1990)
  print(person)
  # Accessing things inside a list --
  print(person$name)
  print(person$x)

  # To make a matrix, pasting together the columns 'year' 'x' and 'y' The verb
  # cbind() stands for 'column bind'
  print(cbind(year, x, y))
  print(cbind(year, x, y, names))

  # To make a 'data frame', which is a list of vectors of the same length --
  d <- data.frame(names, year, x, y)
  print(d)
  print(nrow(d))
  # Accessing one of these vectors
  print(d$names)
  # Accessing the last element of this vector
  print(d$names[nrow(d)])
  # Or equally,
  print(d$names[length(d$names)])
  f <- factor(d$year)
  print(f)
  print(as.numeric(f))
  print(levels(f)[f])
  print(factor(d$x))
  print(factor(d$y))
  return(0)
}

if (identical(environment(), globalenv()))
  quit(status = main(commandArgs(trailingOnly = TRUE)))

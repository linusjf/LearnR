#!/usr/bin/env Rscript
## Constructor function for polygon objects x a numeric vector of x coordinates y
## a numeric vector of y coordinates
make_poly <- function(x, y) {
  if (length(x) != length(y)) {
    stop("'x' and 'y' should be the same length")
  }
  ## Create the 'polygon' object
  object <- list(xcoord = x, ycoord = y)
  ## Set the class name
  class(object) <- "polygon"
  return(object)
}

## Print method for polygon objects x an object of class 'polygon'
print.polygon <- function(x, ...) {
  cat("a polygon with", length(x$xcoord), "vertices\n")
  invisible(x)
}

## Summary method for polygon objects object an object of class 'polygon'
summary.polygon <- function(object, ...) {
  object <- list(rng.x = range(object$xcoord), rng.y = range(object$ycoord))
  class(object) <- "summary_polygon"
  return(object)
}

## Print method for summary.polygon objects x an object of class 'summary_polygon'
print.summary_polygon <- function(x, ...) {
  cat("x:", x$rng.x[1], "-->", x$rng.x[2], "\n")
  cat("y:", x$rng.y[1], "-->", x$rng.y[2], "\n")
  invisible(x)
}

main <- function(argv) {
  ## Construct a new 'polygon' object
  x <- make_poly(1:4, c(1, 5, 2, 1))
  print(x)
  out <- summary(x)
  print(class(out))
  print(out)
  print(summary(x))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

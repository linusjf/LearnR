#!/usr/bin/env Rscript

main <- function(argv) {
  par(mfrow = c(3, 2), cex = 0.7, mar = c(4, 4, 1, 1))
  y <- rnorm(20)
  print(y)
  plot(y, type = "p")
  plot(y, type = "l")
  plot(y, type = "b")
  plot(y, type = "h")
  plot(y, type = "o")
  plot(y, type = "n")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

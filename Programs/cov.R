#!/usr/bin/env Rscript
makecov <- function(rho, n) {
  m <- matrix(nrow = n, ncol = n)
  m <- ifelse(row(m) == col(m), 1, rho)
  return(m)
}

main <- function(argv) {
  acov <- makecov(0.5, 4)
  print(acov)
  bcov <- makecov(0.34, 15)
  print(bcov)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
library(purrr)

main <- function(argv) {
  n_unique <- purrr::compose(length, unique)

  # nolint start The composition above is the same as: n_unique <- function(x) {
  # length(unique(x)) } nolint end

  print(rep(1:5, 1:5))

  print(n_unique(rep(1:5, 1:5)))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

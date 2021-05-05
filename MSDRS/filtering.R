#!/usr/bin/env Rscript
library(purrr)

main <- function(argv) {
  print(keep(1:20, function(x) {
    x%%2 == 0
  }))
  print(discard(1:20, function(x) {
    x%%2 == 0
  }))
  print(every(1:20, function(x) {
    x%%2 == 0
  }))
  print(some(1:20, function(x) {
    x%%2 == 0
  }))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

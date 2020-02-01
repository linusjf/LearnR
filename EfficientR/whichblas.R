#!/usr/bin/env Rscript
library("benchmarkme")

main <- function(argv) {
  print(benchmarkme::get_linear_algebra())
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

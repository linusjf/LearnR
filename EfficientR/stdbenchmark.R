#!/usr/bin/env Rscript
library(benchmarkme)

main <- function(argv) {
  print(benchmarkme::benchmark_std())
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

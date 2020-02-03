#!/usr/bin/env Rscript

main <- function(argv) {
  print(names(methods:::.BasicFunsList))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

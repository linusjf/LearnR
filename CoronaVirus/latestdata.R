#!/usr/bin/env Rscript
library(nCov2019)

main <- function(argv) {
  x <- nCov2019::get_nCov2019(lang = "en")
  print(x)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

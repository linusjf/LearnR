#!/usr/bin/env Rscript
library(nCov2019)

main <- function(argv) {
  x <- nCov2019::get_nCov2019(lang = "en")
  print(x)
  cat("Global data:\n")
  print(x["global", ])
  cat("China provincial data:\n")
  print(x[])
  cat("Hubei data:\n")
  print(x["Hubei", ])
  print(summary(x))

  cat("Historical data:\n")
  history <- nCov2019::load_nCov2019(lang = "en")
  print(history)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

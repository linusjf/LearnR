#!/usr/bin/env Rscript

main <- function(argv) {
  # Note: uses 2+ GB RAM and several seconds or more depending on hardware
  # 1: Create large dataset
  x <- as.data.frame(matrix(rnorm(1e7), nrow = 1e6))
  # 2: Find the median of each column using a single core
  r1 <- lapply(x, median)
  print(r1)
  # 3: Find the median of each column using many cores
  r2 <- parallel::mclapply(x, median)
  print(r2)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

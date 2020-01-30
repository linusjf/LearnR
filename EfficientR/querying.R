#!/usr/bin/env Rscript
library("microbenchmark")

main <- function(argv) {
  df <- data.frame(v = 1:4, name = letters[1:4])
  print(microbenchmark::microbenchmark(
    df[3, 2],
    df[3, "name"],
    df$name[3]
  ))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

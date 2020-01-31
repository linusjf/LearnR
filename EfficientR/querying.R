#!/usr/bin/env Rscript
library("microbenchmark")

main <- function(argv) {
  df <- data.frame(v = 1:4, name = letters[1:4])
  print(microbenchmark::microbenchmark(
    df[3, 2],
    df[3, "name"],
    df$name[3]
  ))
  # Test how long it takes to subset the data frame 50,000 times:
print(system.time(
  for (i in 1:50000) {
    df[3, 2]
  }))
print(system.time(
  for (i in 1:50000) {
    df[3, "name"]
  }))
print(system.time(
  for (i in 1:50000) {
    df$name[3]
  }))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

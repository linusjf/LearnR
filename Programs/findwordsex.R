#!/usr/bin/env Rscript
main <- function(argv) {
  if (!exists("findwords", mode = "function")) {
    source("findwords.R")
  }
  # nolint start
  wrdlist <- findwords("testconcorda.txt")
  sorted <- alphawl(wrdlist)
  print(sorted)
  frqwl <- freqwl(wrdlist)
  # nolint end
  print(frqwl)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

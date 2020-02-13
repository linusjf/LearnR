#!/usr/bin/env Rscript
suppressMessages(library(quantmod))

main <- function(argv) {
  quantmod::getSymbols("AAPL")
  chartSeries(AAPL, subset = "last 4 months")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
suppressMessages(library(quantmod))

main <- function(argv) {
  quantmod::getSymbols("AAPL")
  quantmod::chartSeries(environment()$AAPL, subset = "last 4 months")
  return(0)
}

options("getSymbols.warning4.0" = FALSE)
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

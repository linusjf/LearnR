#!/usr/bin/env Rscript
main <- function(argv) {
  # set graphical output file
  pdf("xh.pdf")
  # generate 100 N(0, 1) variates and plot their histogram
  hist(rnorm(100))
  # close the graphical output file
  graphics.off()
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

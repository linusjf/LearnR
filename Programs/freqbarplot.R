#!/usr/bin/env Rscript
main <- function(argv) {
  if (!exists("findwords", mode = "function")) {
    source("findwords.R")
  }
  # nolint start
  cnn <- findwords("cnna.txt")
  scnn <- freqwl(cnn)
  # nolint end
  nwords <- length(scnn)
  pdf("freqbarplot.pdf")
  scnn90plus <- lengths(scnn[round(0.9 * nwords):nwords])
  print(scnn90plus)
  barplot(scnn90plus)
  graphics.off()
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

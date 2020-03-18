#!/usr/bin/env Rscript
#--------------- Setup--------------------
devtools::install_github("GuangchuangYu/nCov2019",
                         destdir = .libPaths()[1])
library(nCov2019)

main <- function(argv) {
  x <- nCov2019::get_nCov2019(lang = "en")
  cat("Global data:\n")
  print(head(x["global", ], 30))
  print(summary(x))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

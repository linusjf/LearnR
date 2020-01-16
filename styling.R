#!/usr/bin/env Rscript
library(styler)

main <- function(argv) {
  print(sessionInfo())
  path <- dir(path = ".", pattern = "^[A-Z].+$")
  style_dir(path, recursive = TRUE)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

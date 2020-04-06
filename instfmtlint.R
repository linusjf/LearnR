#!/usr/bin/env Rscript

install_tools <- function() {
  if (!require(lintr)) {
    install.packages("lintr", .libPaths()[1])
  }
  if (!require(styler)) {
    install.packages("styler", .libPaths()[1])
  }
  if (!require(startup)) {
    install.packages("startup", .libPaths()[1])
  }
}

main <- function(argv) {
  print(Sys.info())
  print(sessionInfo())
  update.packages()
  suppressWarnings(install_tools())
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript

install_tools <- function() {
  install.packages("lintr", .libPaths()[1])
  install.packages("styler", .libPaths()[1])
  install.packages("formatR", .libPaths()[1])
}

main <- function(argv) {
  install_tools()
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

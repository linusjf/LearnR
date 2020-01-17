#!/usr/bin/env Rscript

install_tools <- function() {
  install.packages("lintr", .libPaths()[1])
  install.packages("styler", .libPaths()[1])
  install.packages("formatR", .libPaths()[1])
}

install_programs <- function() {
  install.packages("stargazer", .libPaths()[1])
  install.packages("compiler", .libPaths()[1])
  install.packages("rbenchmark", .libPaths()[1])
  install.packages("pixmap", .libPaths()[1])
}

install_rbyexample <- function() {
  install.packages("stringi", .libPaths()[1])
  install.packages("xtable", .libPaths()[1])
  install.packages("tseries", .libPaths()[1])
}

main <- function(argv) {
  install_tools()
  install_programs()
  install_rbyexample()
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

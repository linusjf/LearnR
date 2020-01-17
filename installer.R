#!/usr/bin/env Rscript

install_tools <- function() {
  if (!require(lintr)) {
    install.packages("lintr", .libPaths()[1])
  }
  if (!require(styler)) {
    install.packages("styler", .libPaths()[1])
  }
  if (!require(formatR)) {
    install.packages("formatR", .libPaths()[1])
  }
}

install_programs <- function() {
  if (!require(stargazer)) {
    install.packages("stargazer", .libPaths()[1])
  }
  if (!require(compiler)) {
    install.packages("compiler", .libPaths()[1])
  }
  if (!require(rbenchmark)) {
    install.packages("rbenchmark", .libPaths()[1])
  }
  if (!require(pixmap)) {
    install.packages("pixmap", .libPaths()[1])
  }
}

install_rbyexample <- function() {
  if (!require(stringi)) {
    install.packages("stringi", .libPaths()[1])
  }
  if (!require(xtable)) {
    install.packages("xtable", .libPaths()[1])
  }
  if (!require(tseries)) {
    install.packages("tseries", .libPaths()[1])
  }
  if (!require(Hmisc)) {
    install.packages("Hmisc", .libPaths()[1])
  }
}

main <- function(argv) {
  r <- getOption("repos")
  r["CRAN"] <- "http://cran.us.r-project.org"
  options(repos = r)
  install_tools()
  install_programs()
  install_rbyexample()
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

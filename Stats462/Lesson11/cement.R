#!/usr/bin/env Rscript
cement.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/cement.txt")
}

lib_step <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libstep.R")
}

library(skimr)
source(lib_step())
suppressPackageStartupMessages(library(PerformanceAnalytics))

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(cement.txt(), header = TRUE, as.is = TRUE)
  print(head(data))
  print(skimr::skim(data))

  chart.Correlation(data, histogram = TRUE, pch = 15)

  step_wise_regression(data, "y")

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

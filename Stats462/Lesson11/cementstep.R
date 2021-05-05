#!/usr/bin/env Rscript
cement.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/cement.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(PerformanceAnalytics))

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(cement.txt(), header = TRUE, as.is = TRUE)
  print(head(data))
  print(skimr::skim(data))

  chart.Correlation(data, histogram = TRUE, pch = 15)

  model <- lm(y ~ ., data)
  final <- step(model, trace = 10, k = log(nrow(data)), direction = "both", pent = 0.15, 
    prem = 0.15)
  print(final)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

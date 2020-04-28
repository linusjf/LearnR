#!/usr/bin/env Rscript
cement.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/cement.txt"
  )
}

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(olsrr))

main <- function(argv) {
  cement <- read.table(cement.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(cement))
  print(skimr::skim(cement))

  chart.Correlation(cement,
    histogram = TRUE,
    pch = 15
  )
  model <- lm(y ~ ., data = cement)
  k <- ols_step_all_possible(model)
  plot.obj <- plot(k, print_plot = FALSE)
  lapply(plot.obj, print)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

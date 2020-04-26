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

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(cement.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  chart.Correlation(data,
    histogram = TRUE,
    pch = 15
  )

  return(0)
}

step_wise_regression <- function(data,
                                 response,
                                 predictors,
                                 removals,
                                 alpha_remove = 0.15,
                                 alpha_enter = 0.15) {
  if (data == NULL) {
    stop("Data frame expected. Cannot be NULL")
  }
  if (response == NULl | response == "") {
    stop("Response cannot be NULL or empty")
  }
  if (predictors == NULl | length(predictors) == 0) {
    stop("Predictors cannot be NULL or empty")
  }
  if (!response %in% colnames(data)) {
    stop(sprintf("Column %s does not exist", response))
  }
  for (value in predictors) {
    if (!value %in% colnames(data)) {
      stop(sprintf("Column %s does not exist", value))
    }
  }
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

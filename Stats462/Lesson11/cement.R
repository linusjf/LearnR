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

  step_wise_regression(data, "y")
  return(0)
}

step_wise_regression <- function(data,
                                 response,
                                 predictors = NULL,
                                 removals = NULL,
                                 alpha_remove = 0.15,
                                 alpha_enter = 0.15) {
  validate(data = data, response = response, predictors = predictors, removals = removals,
  alpha_remove = alpha_remove, alpha_enter = alpha_enter)
}

validate <- function(...) {
  parms <- list(...)
  if (is.null(parms$data)) {
    stop("Data frame expected. Cannot be NULL")
  }
  if (is.null(parms$response) | parms$response == "") {
    stop("Response cannot be NULL or empty")
  }
  if (!parms$response %in% colnames(parms$data)) {
    stop(sprintf("Column %s does not exist", parms$response))
  }
  if (!is.null(parms$predictors) & length(parms$predictors) > 0) {
    for (value in parms$predictors) {
      if (!value %in% colnames(parms$data)) {
        stop(sprintf("Column %s does not exist", value))
      }
    }
  }
  if (parms$alpha_remove < 0 | parms$alpha_remove > 1) {
    stop("Alpha remove is between 0 and 1 included")
  }
  if (parms$alpha_enter < 0 | parms$alpha_enter > 1) {
    stop("Alpha enter is between 0 and 1 included")
  }
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

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
  validate(data = data, response = response, predictors = predictors, removals =
           removals, alpha_remove = alpha_remove, alpha_enter = alpha_enter)
  models <- list()
  if (is.null(predictors))
    predictors <- colnames(data)
  predictors <- predictors[predictors != response]
  if (!is.null(removals))
    predictors <- predictors[!predictors %in% removals]
  i <- 1
  for (value in predictors) {
    expr <- paste0(response, " ~ ", value)
    model <- eval(parse(text = paste0("lm(", expr,
                ",data)")))
    models[[i]] <- model
    i <- i + 1
  }
  p_vals <- c()
  for (model in models) {
    index <- length(model$coefficients)
    summ <- summary(model)
    p <- summ$coefficients[index, 4]
    p_vals <- c(p_vals, p)
  }
  p_min <- min(p_vals)
  if (p_min < alpha_enter) {
  p_min_index <- which(p_vals == p_min)
  model_chosen <- models[[p_min_index]]
  print("Model chosen: ")
  print(model_chosen)
  } else {
    print(sprintf("No p-value meets criteria of alpha entry = ",
                  alpha_entry))
  }
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

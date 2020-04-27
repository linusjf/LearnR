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
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tibble))

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
                                 formula = NULL,
                                 alpha_remove = 0.15,
                                 alpha_enter = 0.15
                                 ) {
  validate(
    data = data, response = response, predictors = predictors, removals =
      removals, alpha_remove = alpha_remove, alpha_enter = alpha_enter
  )
  models <- list()
  if (is.null(predictors)) {
    predictors <- colnames(data)
  }
  predictors <- predictors[predictors != response]
  if (!is.null(removals)) {
    predictors <- predictors[!predictors %in% removals]
  }
  model_variables <- c()
  if (!is.null(formula)) {
      term <-
           terms(formula, keep.order = TRUE)
  model_variables <- attr(term, "term.labels")
  }
  i <- 1
  for (value in predictors) {
    variables <- c(model_variables, value)
    f <-
      as.formula(paste(response,
                       paste(variables, collapse = " + "),
                       sep = " ~ "))
    model <- lm(f, data)
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
    print("Model chosen currently: ")
    print(model_equation(model_chosen, digits = 4))

    model_variables <-
           model_chosen$coefficients[c(2:
                                     length(model$coefficients))]
    predictors <- predictors[!predictors %in% model_variables]
    formula <- formula(model_chosen)
    removals <- attr(terms(formula), "term.labels")

    print("Checking for impact on p-values")
    summ <- summary(model_chosen)
    coefficients <-
      as.data.frame(summ$coefficients)
    colnames(coefficients) <-
      c("Beta", "SE", "t.val", "p.val")
    coefficients <- tail(coefficients, -1)
    print(coefficients)
    drop <- coefficients %>%
      rownames_to_column("rownames") %>%
      filter(p.val > alpha_remove) %>%
      column_to_rownames("rownames")
    if (nrow(drop) > 0) {
    retain <- coefficients %>%
      rownames_to_column("rownames") %>%
      filter(p.val <= alpha_remove) %>%
      column_to_rownames("rownames")
    removals <- c(removals, rownames(drop))
    retain <-
      rownames(retain)
    formula <-
      as.formula(paste(response,
                       paste(retain, collapse = " + "),
                       sep = " ~ "))
    }
    step_wise_regression(data, response,
    predictors, removals, formula,
    alpha_remove,
    alpha_enter)

  } else {
    print(sprintf(
      "No p-value meets criteria of alpha entry = %f",
      alpha_enter
    ))
    if (!is.null(formula)) {
      print("Final Model chosen: ")
      model <- lm(formula, data)
      print(model_equation(model, digits = 4))
    }
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

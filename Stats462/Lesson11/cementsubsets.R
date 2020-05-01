#!/usr/bin/env Rscript
cement.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/cement.txt"
  )
}, list())

lib_path <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}, list())

library(skimr)
source(lib_path)
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(olsrr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(regclass))
suppressPackageStartupMessages(library(purrr))

main <- function(argv) {
  cement <- read.table(cement.txt,
    header = TRUE, as.is = TRUE
  )
  print(head(cement))
  print(skimr::skim(cement))

  chart.Correlation(cement,
    histogram = TRUE,
    pch = 15
  )
  model <- lm(y ~ ., data = cement)
  best <- ols_step_best_subset(model)
  print(str(best))
  lmset <- fill_models(best, model)
  print(lmset)
  best_rsq <- best_model_rsquare(best, model)
  best_adjr <- best_model_adjrsquare(best, model)
  best_cp <- best_model_cp(best, model, "mincp")
  best_cp2 <- best_model_cp(best, model, "relcp")
  print(best_rsq)
  print(best_adjr)
  print(best_cp)
  print(best_cp2)
  print(compute_cp(model, best_cp))
  print(compute_cp(model, best_cp2))
  best <- unique(list(
    best_rsq,
    best_adjr,
    best_cp,
    best_cp2
  ))
  print(best_vif(best))
  print(best_vif(lmset))

  return(0)
}

best_vif <- function(models, cutoff = 4) {
  selected <- list()
  nvars <- c()
  for (model in models) {
    vif <- NULL
    if (length(model$coefficients) > 2) {
      vif <- VIF(model)
    } else {
      vif <- 1
    }
    coeffs <- names(model$coefficients)[2:length(model$coefficients)]
    df <- data.frame(list(coeffs = coeffs, vif = vif))
    df %<>%
      filter(vif > cutoff)
    if (nrow(df) == 0) {
      nvars <- c(nvars, length(coeffs))
      selected[[length(selected) + 1]] <- model
    }
  }
  if (length(selected) == 1) {
    return(selected)
  } else {
    min_nvars <- min(nvars)
    index <- 1
    for (model in selected) {
      if ((length(model$coefficients) - 1) != min_nvars) {
        selected[[index]] <- NULL
        index <- index + 1
      }
    }
    return(compact(selected))
  }
}

compute_cp <- function(full_model, model) {
  res <- NULL
  if (length(full_model$coefficients) <
    length(model$coefficients)) {
    res <- anova(model, full_model)
  } else {
    res <- anova(full_model, model)
  }
  res %<>%
    mutate(mse = RSS / Res.Df)
  n <- nrow(model$model)
  p <- length(model$coefficients)
  ssek <- res$RSS[2]
  mseall <- res$mse[1]
  cp <- ssek / mseall +
    2 * p - n
  return(cp)
}

fill_models <- function(models, model) {
  if (!inherits(models, "ols_step_best_subset")) {
    stop("Class has to be ols_step_best_subset")
  }
  lmset <- list()
  nrows <- nrow(models)
  for (idx in seq_len(nrows)) {
    df <- models %>%
      filter(mindex == idx)
    predictors <- df$predictors
    rhs <- str_replace_all(predictors, " ", "+")
    formula <- update(formula(model), paste0(". ~ ", rhs))
    data <- model$model
    lmset[[length(lmset) + 1]] <-
      lm(formula, data)
  }
  return(lmset)
}

best_model_rsquare <- function(models, model, rsqinc = 0.05) {
  if (!inherits(models, "ols_step_best_subset")) {
    stop("Class has to be ols_step_best_subset")
  }
  models %<>%
    mutate(rsq.inc = ((rsquare / lag(rsquare) - 1)))
  models %<>%
    filter(rsq.inc >= rsqinc) %>%
    filter(n == max(n))
  predictors <- models$predictors
  rhs <- str_replace_all(predictors, " ", "+")
  formula <- update(formula(model), paste0(". ~ ", rhs))
  data <- model$model
  return(lm(formula, data))
}

best_model_adjrsquare <- function(models, model) {
  if (!inherits(models, "ols_step_best_subset")) {
    stop("Class has to be ols_step_best_subset")
  }
  models %<>%
    filter(adjr == max(adjr))
  predictors <- models$predictors
  rhs <- str_replace_all(predictors, " ", "+")
  formula <- update(formula(model), paste0(". ~ ", rhs))
  data <- model$model
  return(lm(formula, data))
}

best_model_cp <- function(models, model, criteria = "mincp") {
  if (!inherits(models, "ols_step_best_subset")) {
    stop("Class has to be ols_step_best_subset")
  }
  data <- model$model
  ncoefs <- length(model$coefficients)
  models %<>%
    mutate(p = n + 1) %>%
    mutate(cpratio = cp / p) %>%
    filter(p != ncoefs)
  if (criteria == "mincp") {
    models %<>%
      filter(cp == min(cp))
  } else if (criteria == "relcp") {
    models %<>%
      filter(cpratio == min(cpratio))
  } else {
    stop("Criteria should be mincp or relcp")
  }
  models %<>%
    filter(n == min(n))
  predictors <- models$predictors
  rhs <- str_replace_all(predictors, " ", "+")
  formula <- update(formula(model), paste0(". ~ ", rhs))
  return(lm(formula, data))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

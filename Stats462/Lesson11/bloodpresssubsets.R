#!/usr/bin/env Rscript
bloodpress.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/bloodpress.txt"
  )
}, list())

lib_path <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}, list())

lib_step <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libstep.R"
  )
}, list())

library(skimr)
source(lib_path)
source(lib_step)
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(nortest))

main <- function(argv) {
  datum <- read.table(bloodpress.txt,
    header = TRUE, as.is = TRUE
  )
  print(head(datum))
  print(skimr::skim(datum))

  chart.Correlation(datum,
    histogram = TRUE,
    pch = 15
  )
  model <- lm(BP ~ ., data = datum)
  best <- ols_step_all_possible(model)
  print(str(best))
  lmset <- fill_models(best, model)
  print("All possible subsets")
  print(lmset)
  N <- nrow(datum)
  t <- sqrt(log(N))
  k <- ncol(datum) - 1
  p <- 2 * pt(-abs(t), df = N - k - 1)
  best_p <- step_wise_regression(datum, "BP")
  best_p2 <- step_wise_regression(datum, "BP",
    alpha_entry = p, alpha_removal = p
  )
  best_rsq <- best_model_rsquare(best, model)
  best_adjr <- best_model_adjrsquare(best, model)
  best_cp <- best_model_cp(best, model, "mincp")
  best_cp2 <- best_model_cp(best, model, "relcp")
  print("Best p model(s)")
  print(best_p)
  print(best_p2)
  print("Best rsquared model(s)")
  print(best_rsq)
  print("Best adj rsquared model(s)")
  print(best_adjr)
  print("Best cp model(s)")
  print(best_cp)
  print(best_cp2)
  print("Computed cp(s)")
  print(compute_cp(model, best_cp))
  print(compute_cp(model, best_cp2))
  best <- unique_models(list(
    best_p,
    best_p2,
    best_rsq,
    best_adjr,
    best_cp,
    best_cp2
  ))
  print("Best subset using p, rsquare, adjr and cp")
  print(best)
  lapply(best, checkfit)
  print("Best vif model(s)")
  models <- best_vif(best)
  print(models)

  lapply(models, checkfit)
  return(0)
}

unique_models <- function(models) {
  if (!inherits(models, "list"))
    stop("Expecting a list object.")
  if (length(models) == 0) {
    return(models)
  }
  classname <- "lm"
  class <- c()
  for (val in models) {
    class <- c(class, class(val))
  }
  if (!(length(intersect(class, class)) == 1 &
    class[1] == classname)) {
    stop("Not all models are 'lm' objects")
  }
  env <- new.env(hash = TRUE)
  lapply(models, add_to_env, env)
  return(as.list(env))
}

add_to_env <- function(model, env) {
  coefficients <- names(model$coefficients)[2:length(model$coefficients)]
  coefficients <- sort(coefficients)
  key <- paste(coefficients, collapse = "")
  expr <- paste0("env$", key, " <- model")
  eval(parse(text = expr))
}

checkfit <- function(model) {
  eqn <- model_equation(model, digits = 4)
  frm <- format(formula(model))
  plot(model,
    which = c(1, 2),
    caption = list("Residuals vs Fitted", "Normal Q-Q"),
    sub.caption = list(frm, frm)
  )
  ad <- ad.test(resid(model))
  print(eqn)
  print(ad)
  if (ad$p.value < 0.05) {
    print("Model rejected: Residuals are non-normal")
  }
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

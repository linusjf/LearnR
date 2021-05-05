#!/usr/bin/env Rscript
peru.txt <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/peru.txt")
}, list())

lib_path <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}, list())

lib_step <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libstep.R")
}, list())

library(skimr)
source(lib_path)
source(lib_step)
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(nortest))

main <- function(argv) {
  data <- read.table(peru.txt, header = TRUE, as.is = TRUE)
  data %<>%
    mutate(fraclife = Years/Age) %>%
    select(Age, Years, fraclife, Weight, Height, Chin, Forearm, Pulse, Systol)
  print(head(data))
  print(skimr::skim(data))

  chart.Correlation(data, histogram = TRUE, pch = 15)
  model <- lm(Systol ~ ., data = data)
  all <- ols_step_all_possible(model)
  print(str(all))
  lmset <- fill_models(all, model)
  print("All possible subsets")
  print(lmset)
  N <- nrow(data)
  t <- sqrt(log(N))
  k <- ncol(data) - 1
  p <- 2 * pt(-abs(t), df = N - k - 1)
  print("p-value for Schwarz Bayesian equivalent")
  print(p)
  best_p <- step_wise_regression(data, "Systol")
  best_p2 <- step_wise_regression(data, "Systol", alpha_entry = p, alpha_removal = p)
  best_rsq <- best_model_rsquare(all, model)
  best_adjr <- best_model_adjrsquare(all, model)
  best_cp <- best_model_cp(all, model, "mincp")
  best_cp2 <- best_model_cp(all, model, "relcp")
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
  best_aic <- best_model_aic(all, model)
  best_sbc <- best_model_sbc(all, model)
  print("Best information model(s)")
  print(best_aic)
  print(best_sbc)
  print("Computed cp(s)")
  print(compute_cp(model, best_cp))
  print(compute_cp(model, best_cp2))
  best <- unique_models(list(best_p, best_p2, best_rsq, best_adjr, best_cp, best_cp2, 
    best_aic, best_sbc))
  print("Best subset using p, rsquare, adjr ,cp, aic, sbc")
  print(best)
  lapply(best, checkfit)
  print("Best vif model(s)")
  models <- best_vif(best)
  print(models)

  lapply(models, checkfit)
  return(0)
}

checkfit <- function(model) {
  eqn <- model_equation(model, digits = 4)
  frm <- format(formula(model))
  plot(model, which = c(1, 2), caption = list("Residuals vs Fitted", "Normal Q-Q"), 
    sub.caption = list(frm, frm))
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

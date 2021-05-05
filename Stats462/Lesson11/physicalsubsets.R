#!/usr/bin/env Rscript
Physical.txt <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/Physical.txt")
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
  data <- read.table(Physical.txt, header = TRUE, as.is = TRUE)
  data %<>%
    mutate(Gender = ifelse(data$Sex == "Male", 0, 1)) %>%
    select(Height, LeftArm, LeftFoot, LeftHand, HeadCirc, nose, Gender)
  print(head(data))
  print(skimr::skim(data))

  chart.Correlation(data, histogram = TRUE, pch = 15)
  model <- lm(Height ~ ., data = data)
  all <- ols_step_all_possible(model)
  print(str(all))
  lmset <- fill_models(all, model)
  print("All possible subsets")
  print(lmset)
  N <- nrow(data)
  t <- sqrt(log(N))
  k <- ncol(data) - 1
  p <- 2 * pt(-abs(t), df = N - k - 1)
  print("p-value for Schwartz Bayesian equivalent")
  print(p)
  best_p <- step_wise_regression(data, "Height")
  best_p2 <- step_wise_regression(data, "Height", alpha_entry = p, alpha_removal = p)
  print("Best p model(s)")
  print(best_p)
  print(best_p2)
  best_aic <- best_model_aic(all, model)
  best_sbc <- best_model_sbc(all, model)
  print("Best information model(s)")
  print(best_aic)
  print(best_sbc)
  best <- unique_models(list(best_p, best_p2, best_aic, best_sbc))
  print("Best subset using p, aic, sbc")
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
  residuals <- resid(model)
  ad <- ad.test(residuals)
  print(eqn)
  print(ad)
  if (ad$p.value < 0.05) {
    print("Model rejected: Residuals are non-normal")
  }
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

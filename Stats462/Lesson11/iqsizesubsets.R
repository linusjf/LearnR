#!/usr/bin/env Rscript
iqsize.txt <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/iqsize.txt")
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
  datum <- read.table(iqsize.txt, header = TRUE, as.is = TRUE)
  print(head(datum))
  print(skimr::skim(datum))

  chart.Correlation(datum, histogram = TRUE, pch = 15)
  model <- lm(PIQ ~ ., data = datum)
  best <- ols_step_all_possible(model)
  print(str(best))
  lmset <- fill_models(best, model)
  print("All possible subsets")
  print(lmset)
  best_rsq <- best_model_rsquare(best, model)
  best_adjr <- best_model_adjrsquare(best, model)
  best_cp <- best_model_cp(best, model, "mincp")
  best_cp2 <- best_model_cp(best, model, "relcp")
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
  best <- unique(list(best_rsq, best_adjr, best_cp, best_cp2))
  print("Best vif model(s)")
  print(best_vif(best))
  print("Best vif model(s) from all possible")
  models <- best_vif(lmset)
  print(models)

  lapply(models, checkfit)
  return(0)
}

checkfit <- function(model) {
  plot(model, which = c(1, 2), caption = list("Residuals vs Fitted", "Normal Q-Q"))
  ad <- ad.test(resid(model))
  print(model_equation(model, digits = 4))
  print(ad)
  if (ad$p.value < 0.05) {
    print("Model rejected: Residuals are non-normal")
  }
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

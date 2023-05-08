#!/usr/bin/env Rscript
realestate.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/realestate.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))

main <- function(argv) {
  data <- read.table(realestate.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  attach(data)
  model.1 <- lm(SalePrice ~ SqFeet + Lot, data)
  print(anova(model.1))
  print(model_fit_stats(model.1))
  print(model_coeffs(model.1))
  eqn.1 <- model_equation(model.1, digits = 4)
  print(eqn.1)

  detach(data)

  data %<>%
    mutate(lnSalePrice = log(SalePrice)) %>%
    mutate(lnSqFeet = log(SqFeet)) %>%
    mutate(lnLot = log(Lot))

  attach(data)
  model.log <- lm(lnSalePrice ~ lnSqFeet + lnLot, data)
  print(anova(model.log))
  print(model_fit_stats(model.log))
  print(model_coeffs(model.log))
  eqn.log <- model_equation(model.log, digits = 4)
  print(eqn.log)

  detach(data)

  data %<>%
    mutate(residuals = resid(model.log)) %>%
    mutate(fitted = fitted(model.log))

  attach(data)
  plot(fitted, residuals,
    pch = 15, col = "blue", main = "OLS Residuals versus Fitted",
    xlab = "Fitted", ylab = "Residuals", sub = eqn.log, col.sub = "blue"
  )
  detach(data)

  data %<>%
    mutate(absres = abs(residuals))

  attach(data)
  model.absres <- lm(absres ~ fitted, data)
  print(anova(model.absres))
  print(model_fit_stats(model.absres))
  print(model_coeffs(model.absres))
  eqn.absres <- model_equation(model.absres, digits = 4)
  print(eqn.absres)
  detach(data)

  data %<>%
    mutate(fits = fitted(model.absres))

  attach(data)
  model.wls <- lm(lnSalePrice ~ lnSqFeet + lnLot, weights = 1 / fits^2, data)
  print(anova(model.wls))
  print(model_fit_stats(model.wls))
  print(model_coeffs(model.wls))
  eqn.wls <- model_equation(model.wls, digits = 4)
  print(eqn.wls)
  plot(fitted(model.wls), rstandard(model.wls),
    pch = 15, col = "red", main = "WLS Standardized Residuals versus Fitted",
    xlab = "Fitted", ylab = "Standardized Residuals", sub = eqn.wls, col.sub = "red"
  )
  detach(data)

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

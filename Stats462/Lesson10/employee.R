#!/usr/bin/env Rscript
employee.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/employee.txt"
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
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(FitAR))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(Hmisc))
library(orcutt)

ols_analysis <- function(data) {
  model <- lm(metal ~ vendor, data)

  print(model_coeffs(model))
  print(model_fit_stats(model))
  eqn <- model_equation(
    model,
    digits = 4
  )
  print(eqn)

  coeffs <- summary(model)$coefficients
  intercept <- coeffs[1, 1]
  slope <- coeffs[2, 1]
  with(
    data,
    plot(vendor, metal,
      pch = 15,
      col = "blue", main = eqn,
      col.main = "red", sub = "Ordinary Least Squares"
    )
  )
  abline(model, col = "red")
  return(model)
}

cochrane_orcutt <- function(model, data) {
  data %<>%
    mutate(residuals = resid(model)) %>%
    mutate(lag1residuals = Lag(residuals))
  residmodel <- lm(residuals ~ lag1residuals - 1, data)

  rho <- residmodel$coefficients[1]

  data %<>%
    mutate(Y_co = metal - rho * Lag(metal)) %>%
    mutate(X_co = vendor - rho * Lag(vendor))

  lagmodel <- lm(Y_co ~ X_co, data)

  print(model_coeffs(lagmodel))
  print(model_fit_stats(lagmodel))
  print(model_equation(lagmodel, digits = 4))

  print(dwtest(lagmodel))

  coeffs <- summary(lagmodel)$coefficients
  intercept <- coeffs[1, 1] / (1 - rho)
  print(intercept)
  intercept.se <- coeffs[1, 2] / (1 - rho)
  print(intercept.se)
  slope <- coeffs[2, 1]

  data %<>%
    mutate(fitted.cochrane1 = intercept + slope * vendor) %>%
    mutate(e.cochrane1 = metal - fitted.cochrane1) %>%
    mutate(forecast.cochrane1 = fitted.cochrane1 + rho * Lag(e.cochrane1))
  eqn <- paste0(
    "metal = ",
    round(intercept, 4), " + ",
    round(slope, 4), " * vendor"
  )
  with(
    data,
    plot(vendor, metal,
      pch = 15,
      col = "blue", main = eqn,
      col.main = "red", sub = "Cochrane Orcutt 1 iteration"
    )
  )
  with(data, {
    lo <- lm(forecast.cochrane1 ~ vendor)
    abline(lo, col = "red")
  })
}

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(employee.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  model <- ols_analysis(data)
  test_autocorrelations(model, data)
  cochrane_orcutt(model, data)
  return(0)
}

test_autocorrelations <- function(model, data) {
  data %<>%
    mutate(residuals = resid(model))
  with(data, {
    plot(time, residuals,
      main = "Residuals",
      xlab = "Order (time)",
      ylab = "Residuals"
    )
    abline(h = 0)
  })
  with(
    data,
    Pacf(residuals)
  )
  print(dwtest(model))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

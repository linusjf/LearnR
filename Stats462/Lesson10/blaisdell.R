#!/usr/bin/env Rscript
blaisdell.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/blaisdell.txt"
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
  model <- lm(comsales ~ indsales, data)

  print(model_coeffs(model))
  print(model_fit_stats(model))
  eqn <- model_equation(
  model, digits = 4)
  print(eqn)

  coeffs <- summary(model)$coefficients
  intercept <- coeffs[1, 1]
  slope <- coeffs[2, 1]
  with(data,
  plot(indsales, comsales,
    pch = 15,
    col = "blue", main = eqn,
    col.main = "red", sub = "Ordinary Least Squares"
  ))
  abline(model, col = "red")
  return(model)
}

ljung_boxq <- function(model, data) {
  print(dwtest(model))

  # Ljung - BoxQ test
  with(
    data,
    print(Box.test(residuals, type = "Ljung-Box"))
  )

  with(data, {
    x <- LjungBoxTest(residuals,
      k = 2,
      StartLag = 1
    )
    plot(x[, 3],
      main = "Ljung-Box Q Test",
      col = "blue", pch = 15,
      ylab = "P-values",
      xlab = "Lag"
    )
  })
}

cochrane_orcutt <- function(data) {

  data %<>%
    mutate(lag1residuals = Lag(residuals, 1))
  residmodel <- lm(residuals ~ lag1residuals - 1, data)

  rho <- residmodel$coefficients[1]

  data %<>%
    mutate(Y_co = comsales - rho * Lag(comsales, 1)) %>%
    mutate(X_co = indsales - rho * Lag(indsales, 1))

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
    mutate(fitted.cochrane1 = intercept + slope * indsales) %>%
    mutate(e.cochrane1 = comsales - fitted.cochrane1) %>%
    mutate(forecast.cochrane1 = comsales + slope * Lag(e.cochrane1))
  eqn <- paste0(
    "comsales = ",
    round(intercept, 4), " + ",
    round(slope, 4), " * indsales"
  )
  with(data,
  plot(indsales, comsales,
    pch = 15,
    col = "blue", main = eqn,
    col.main = "red", sub = "Cochrane Orcutt 1 iteration"
  ))
  with(data, {
    lo <- lm(forecast.cochrane1 ~ indsales)
    abline(lo, col = "red")
  })
}

cochrane_orcutt_convergence <- function(model, data) {
  coch <- cochrane.orcutt(model, max.iter = 1000)
  print(coch)
  coeffs <- coch$coefficients
  intercept <- coeffs[1]
  slope <- coeffs[2]
  data %<>%
    mutate(fitted.cochrane = intercept + slope * indsales) %>%
    mutate(e.cochrane = comsales - fitted.cochrane) %>%
    mutate(forecast.cochrane = comsales + slope * Lag(e.cochrane))
  eqn <- paste0(
    "comsales = ",
    round(intercept, 4), " + ",
    round(slope, 4), " * indsales"
  )
  with(data,
  plot(indsales, comsales,
    pch = 15,
    col = "blue", main = eqn,
    col.main = "red", sub = "Cochrane Orcutt convergence"
  ))
  with(data, {
    lo <- lm(forecast.cochrane ~ indsales)
    abline(lo, col = "red")
  })
}

hildreth_lu_analysis <- function(data) {

  rho <- seq(from = 0.01, to = 1, by = 0.01)
  intercept <- NULL
  slope <- NULL
  with(data, {
  parms <- hildreth_lu(comsales, indsales, rho)
  leastssemodel <- parms[[1]]
  rho_val <- parms[[2]]
  print(anova(leastssemodel))
  print(dwtest(leastssemodel))

  coeffs <- leastssemodel$coefficients
  intercept <<- coeffs[1] / (1 - rho_val)
  slope <<- coeffs[2]
  })
  data %<>%
    mutate(fitted.hildrethlu = intercept + slope * indsales) %>%
    mutate(e.hildrethlu = comsales - fitted.hildrethlu) %>%
    mutate(forecast.hildrethlu = comsales + slope * Lag(e.hildrethlu))
  eqn <- paste0(
    "comsales = ",
    round(intercept, 4), " + ",
    round(slope, 4), " * indsales"
  )
  with(data, {
  plot(indsales, comsales,
    pch = 15,
    col = "blue", main = eqn,
    col.main = "red", sub = "Hildreth Ru Least SSE for rho"
  )
  lo <- lm(forecast.hildrethlu ~ indsales)
  abline(lo, col = "red")
  })
}

main <- function(argv) {
  data <- read.table(blaisdell.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  model <- ols_analysis(data)

  data %<>%
    mutate(residuals = resid(model))

  ljung_boxq(model, data)

  cochrane_orcutt(data)

  cochrane_orcutt_convergence(model, data)

  hildreth_lu_analysis(data)

  first_differences_analysis(data)
  return(0)
}

first_differences_analysis <- function(data) {

  intercept <- NULL
  slope <- NULL
  with(data, {
  parms <- first_differences(comsales, indsales, 0.01)
  intercept <<- parms[1]
  slope <<- parms[2]
  })
  data %<>%
    mutate(fitted.firstdiff = intercept + slope * indsales) %>%
    mutate(e.firstdiff = comsales - fitted.firstdiff) %>%
    mutate(forecast.firstdiff = comsales + slope * Lag(e.firstdiff))
  eqn <- paste0(
    "comsales = ",
    round(intercept, 4), " + ",
    round(slope, 4), " * indsales"
  )
  with(data, {
  plot(indsales, comsales,
    pch = 15,
    col = "blue", main = eqn,
    col.main = "red", sub = "First differences method")
  lo <- lm(forecast.firstdiff ~ indsales)
  abline(lo, col = "red")
  }
  )
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

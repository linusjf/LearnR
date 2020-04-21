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

main <- function(argv) {
  data <- read.table(blaisdell.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  model <- lm(comsales ~ indsales, data)

  print(model_coeffs(model))
  print(model_fit_stats(model))
  print(model_equation(model, digits = 4))

  print(dwtest(model))

  # Ljung - BoxQ test
  data %<>%
    mutate(residuals = resid(model))
  with(data,
  print(Box.test(residuals, type = "Ljung-Box"))
  )

  attach(data)
  x <- LjungBoxTest(residuals,
                    k = 2,
                    StartLag = 1)
  plot(x[, 3],
       main = "Ljung-Box Q Test",
       col = "blue", pch = 15,
       ylab = "P-values",
       xlab = "Lag")
  detach(data)

  data %<>%
    mutate(lag1residuals = Lag(residuals, 1))
  residmodel <- lm(residuals ~ lag1residuals - 1, data)

  rho <- residmodel$coefficients[1]
  print(rho)

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

  attach(data)
  data %<>%
    mutate(fitted.cochrane1 = intercept + slope * indsales) %>%
    mutate(e.cochrane1 = comsales - fitted.cochrane1) %>%
    mutate(forecast.cochrane1 = comsales + slope * Lag(e.cochrane1))
  eqn <- paste0("comsales = ",
                round(intercept, 4), " + ",
                round(slope, 4), " * indsales")
  plot(indsales, comsales, pch = 15,
  col = "blue", main = eqn,
  col.main = "red", sub = "Cochrane Orcutt 1 iteration")
  with(data, {
  lo <- lm(forecast.cochrane1 ~ indsales)
  abline(lo, col = "red")
  })

  coch <- cochrane.orcutt(model, max.iter = 1000)
  print(coch)
  coeffs <- coch$coefficients
  intercept <- coeffs[1]
  slope <- coeffs[2]
  data %<>%
    mutate(fitted.cochrane = intercept + slope * indsales) %>%
    mutate(e.cochrane = comsales - fitted.cochrane) %>%
    mutate(forecast.cochrane = comsales + slope * Lag(e.cochrane))
  eqn <- paste0("comsales = ",
                round(intercept, 4), " + ",
                round(slope, 4), " * indsales")
  plot(indsales, comsales, pch = 15,
  col = "blue", main = eqn,
  col.main = "red", sub = "Cochrane Orcutt convergence")
  with(data, {
  lo <- lm(forecast.cochrane ~ indsales)
  abline(lo, col = "red")
  }
  )

  rho <- seq(from = 0.01, to = 1, by = 0.01)
  parms <- hildreth_lu(comsales, indsales, rho)
  leastssemodel <- parms[[1]]
  rho_val <- parms[[2]]
  print(anova(leastssemodel))
  print(dwtest(leastssemodel))

  coeffs <- leastssemodel$coefficients
  intercept <- coeffs[1] / (1 - rho_val)
  slope <- coeffs[2]
  data %<>%
    mutate(fitted.hildrethru = intercept + slope * indsales) %>%
    mutate(e.hildrethru = comsales - fitted.hildrethru) %>%
    mutate(forecast.hildrethru = comsales + slope * Lag(e.hildrethru))
  eqn <- paste0("comsales = ",
                round(intercept, 4), " + ",
                round(slope, 4), " * indsales")
  plot(indsales, comsales, pch = 15,
  col = "blue", main = eqn,
  col.main = "red", sub = "Hildreth Ru Least SSE for rho")
  with(data, {
  lo <- lm(forecast.hildrethru ~ indsales)
  abline(lo, col = "red")
  }
  )

  parms <- first_differences(comsales, indsales, 0.01)
  intercept <- parms[1]
  slope <- parms[2]
  data %<>%
    mutate(fitted.firstdiff = intercept + slope * indsales) %>%
    mutate(e.firstdiff = comsales - fitted.firstdiff) %>%
    mutate(forecast.firstdiff = comsales + slope * Lag(e.firstdiff))
  eqn <- paste0("comsales = ",
                round(intercept, 4), " + ",
                round(slope, 4), " * indsales")
  plot(indsales, comsales, pch = 15,
  col = "blue", main = eqn,
  col.main = "red", sub = "First differences method")
  with(data, {
  lo <- lm(forecast.firstdiff ~ indsales)
  abline(lo, col = "red")
  }
  )

  detach(data)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

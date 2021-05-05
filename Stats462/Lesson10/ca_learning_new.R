#!/usr/bin/env Rscript
# nolint start
ca_learning_new.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/ca_learning_new.txt")
}
# nolint end

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  # read and display data
  data <- read.table(ca_learning_new.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  attach(data)

  # plot simple scatterplot
  plot(num.responses, cost, main = "Scatterplot of num.responses versus cost", 
    xlab = "num.responses", ylab = "cost", col = "blue", pch = 15)

  # simple model
  model.1 <- lm(cost ~ num.responses, data)
  print(summary(model.1))
  eqn1 <- model_equation(model.1, digits = 4)
  print(eqn1)
  detach(data)

  # add residuals
  data %<>%
    mutate(residuals = resid(model.1))

  # plot residuals versus num.responses (predictor)
  attach(data)
  plot(num.responses, residuals, main = "Scatterplot of Residuals versus num.responses", 
    xlab = "num.responses", ylab = "Residuals", col = "blue", pch = 15)
  abline(h = 0)
  detach(data)

  # add absolute residuals
  data %<>%
    mutate(absres = abs(residuals))

  # plot absolute residuals versus num.responses
  attach(data)
  plot(num.responses, absres, main = "Scatterplot of Absolute Residuals versus num.responses", 
    xlab = "num.responses", ylab = "Absolute Residuals", col = "blue", pch = 15)

  # model absolute residuals versus num.responses
  model.absres <- lm(absres ~ num.responses, data)
  summary(model.absres)
  eqn_absres <- model_equation(model.absres, digits = 4)
  print(eqn_absres)

  detach(data)

  # add fitted absolute residuals
  data %<>%
    mutate(fitted_absres = fitted(model.absres))

  attach(data)

  # model weighted least squares
  model.2 <- lm(cost ~ num.responses, weights = 1/fitted_absres^2, data)
  summary(model.2)
  eqn2 <- model_equation(model.2, digits = 4)
  print(eqn2)

  plot(x = num.responses, y = cost, ylim = c(min(cost), max(cost)), panel.last = c(lines(sort(num.responses), 
    fitted(model.1)[order(num.responses)], col = "blue"), lines(sort(num.responses), 
    fitted(model.2)[order(num.responses)], col = "red")), col.main = "blue", 
    col.sub = "red", main = eqn1, sub = eqn2)
  legend("topleft", col = c("blue", "red"), lty = 1, inset = 0.02, legend = c("OLS", 
    "WLS"))

  # plot standardised residuals for wls
  plot(num.responses, rstandard(model.2), main = "Scatterplot of Standardised Residuals versus num.responses", 
    xlab = "num.responses", ylab = "Standardised Residuals", col = "blue", pch = 15, 
    sub = eqn2, col.sub = "red")
  abline(h = 0)

  detach(data)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

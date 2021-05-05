#!/usr/bin/env Rscript
marketshare.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/marketshare.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())

main <- function(argv) {
  data <- read.table(marketshare.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  attach(data)
  model.1 <- lm(MarketShare ~ Price + P1 + P2, data)
  print(anova(model.1))
  print(model_fit_stats(model.1))
  print(model_coeffs(model.1))
  eqn.1 <- model_equation(model.1, digits = 4)
  print(eqn.1)
  detach(data)

  # plot scatter plot for residuals versus fitted values
  data %<>%
    mutate(residuals = resid(model.1)) %>%
    mutate(fitted = fitted(model.1))
  no_discounts <- data %>%
    filter(Discount == 0)
  discounts <- data %>%
    filter(Discount == 1)
  with(no_discounts, plot(fitted, residuals, col = "blue", pch = 15, main = "OLS Scatterplot of residuals versus fits", 
    xlab = "Fitted", ylab = "Residuals", xlim = c(min(data$fitted), max(data$fitted)), 
    ylim = c(min(data$residuals), max(data$residuals)), sub = eqn.1, col.sub = "blue"))
  with(discounts, points(fitted, residuals, col = "red", pch = 15))
  abline(h = 0)

  var_discounts <- sd(discounts$residuals)^2
  var_no_discounts <- sd(no_discounts$residuals)^2

  weights <- ifelse(data$Discount == 0, 1/var_no_discounts, 1/var_discounts)
  print(weights)
  model.2 <- lm(MarketShare ~ Price + P1 + P2, weights = weights, data)
  print(anova(model.2))
  print(model_fit_stats(model.2))
  print(model_coeffs(model.2))
  eqn.2 <- model_equation(model.2, digits = 4)
  print(eqn.2)

  # plot scatter plot for residuals versus fitted values wls
  data %<>%
    mutate(residuals.wls = rstandard(model.2)) %>%
    mutate(fitted.wls = fitted(model.2))
  no_discounts <- data %>%
    filter(Discount == 0)
  discounts <- data %>%
    filter(Discount == 1)
  with(no_discounts, plot(fitted.wls, residuals.wls, col = "blue", pch = 15, main = "WLS Scatterplot of standardized residuals versus fits", 
    xlab = "Fitted", ylab = "Standardized Residuals", xlim = c(min(data$fitted.wls), 
      max(data$fitted.wls)), ylim = c(min(data$residuals.wls), max(data$residuals.wls)), 
    sub = eqn.2, col.sub = "red"))
  with(discounts, points(fitted.wls, residuals.wls, col = "red", pch = 15))
  abline(h = 0)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

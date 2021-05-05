#!/usr/bin/env Rscript
poisson.txt <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/poisson_simulated.txt")
}, list())

libfunc <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}, list())

library(skimr)
source(libfunc)
suppressPackageStartupMessages(library(survey))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(finalfit))
suppressPackageStartupMessages(library(oddsratio))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(ResourceSelection))
suppressPackageStartupMessages(library(rsq))

main <- function(argv) {
  options(warn = 1)
  data <- read.table(poisson.txt, header = TRUE, as.is = TRUE)
  print(head(data))
  print(skimr::skim(data))
  with(data, plot(x, y, main = "Simulated Poisson Data", pch = 15, col = "blue"))
  model <- glm(y ~ x, data = data, family = "poisson")
  print(model)
  print(summary(model))
  result <- regTermTest(model, formula(model), method = "Wald")
  print(result)
  null_model <- glm(y ~ 1, data = data, family = "poisson")
  print(lrtest(null_model, model))
  print(null_model$deviance - model$deviance)
  print(null_model)
  print(summary(null_model))
  print(anova(null_model, model, test = "Chisq"))
  print(anova(null_model, model, test = "Rao"))
  XBeta <- model.matrix(model) %*% coef(model)
  lambda <- exp(XBeta)
  data %<>%
    mutate(lambda = lambda) %>%
    mutate(prob = (exp(-lambda) * (lambda^y))/factorial(y)) %>%
    mutate(fitted = model$fitted.values)
  print(data)

  n <- nrow(data)
  p <- length(model$coefficients)

  with(data, {
    log_likelihood <- log(prod(prob))
    print(log_likelihood)

    log.lik.model <- logLik(model)
    print(log.lik.model)

    log.lik.nullmodel <- logLik(null_model)
    print(log.lik.nullmodel)

    GSQUARE <- 2 * (as.numeric(log.lik.model) - as.numeric(log.lik.nullmodel))
    print(GSQUARE)

    print(1 - pchisq(GSQUARE, df = 1))

    pearson.statistic <- sum((y - lambda)^2/lambda)

    print(1 - pchisq(pearson.statistic, df = n - p))

    terms <- ifelse(y == 0, 0 - (y - lambda), y * log(y/lambda) - (y - lambda))
    deviance.statistic <- 2 * sum(terms)
    print(1 - pchisq(deviance.statistic, df = n - p))
  })

  null.deviance <- model$null.deviance
  residual.deviance <- model$deviance

  rsquare <- 1 - (residual.deviance/null.deviance)
  print(rsquare)

  rsquared_vals <- lapply(c("v", "kl", "sse", "lr", "n"), rsquared, model)
  print(rsquared_vals)

  data %<>%
    mutate(raw.residuals = y - lambda)
  phi <- NULL
  with(data, phi <<- ((n - p)^(-1)) * sum(((y - lambda)^2)/lambda))
  data %<>%
    mutate(pearson.residuals = raw.residuals/sqrt(phi * lambda)) %>%
    mutate(root.term = ifelse(y == 0, lambda - y, y * log(y/lambda) - (y - lambda))) %>%
    mutate(deviance.residuals = sign(y - lambda) * sqrt(2 * root.term))
  data$root.term <- NULL
  with(data, {
    plot(log(fitted), pearson.residuals, pch = 15, col = "blue", xlab = "Fitted values", 
      ylab = "Pearson residuals", main = "Versus Log of Fits", sub = "(response is y)", 
      ylim = c(-2, 2))
    abline(h = 0, col = "red")
    plot(log(fitted), deviance.residuals, pch = 15, col = "blue", xlab = "Fitted values", 
      ylab = "Deviance residuals", main = "Versus Log of Fits", sub = "(response is y)", 
      ylim = c(-2, 2))
    abline(h = 0, col = "red")
  })
  data %<>%
    mutate(hat.values = hatvalues(model))
  with(data, {
    plot(seq_len(n), hat.values, pch = 15, col = "blue", xlab = "Observations", 
      ylab = "Hat values", main = "Versus Observations", sub = "(response is y)")
    abline(h = 2 * p/n, col = "red")
  })
  data %<>%
    mutate(studentized.pearson = pearson.residuals/sqrt(1 - hat.values)) %>%
    mutate(studentized.deviance = deviance.residuals/sqrt(1 - hat.values))
  with(data, {
    plot(log(fitted), studentized.pearson, pch = 15, col = "blue", xlab = "Fitted values", 
      ylab = "Studentized Pearson residuals", main = "Versus Log of Fits", 
      sub = "(response is y)", ylim = c(-2, 2))
    abline(h = 0, col = "red")
    plot(log(fitted), studentized.deviance, pch = 15, col = "blue", xlab = "Fitted values", 
      ylab = "Studentized Deviance residuals", main = "Versus Log of Fits", 
      sub = "(response is y)")
    abline(h = 0, col = "red")
  })
  df <- data %>%
    filter(hat.values > 2 * p/n)
  print(df)
  return(0)
}

rsquared <- function(type, model) {
  rsquare <- rsq(model, type = type)
  longname <- case_when(type == "v" ~ "Variance function based", type == "kl" ~ 
    "KL divergence based", type == "sse" ~ "SSE based", type == "lr" ~ "Likelihood Ratio Based", 
    type == "n" ~ "Nagelkerke")
  names(rsquare) <- longname
  return(rsquare)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

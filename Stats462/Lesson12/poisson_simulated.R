#!/usr/bin/env Rscript
poisson.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/poisson_simulated.txt"
  )
}, list())

libfunc <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
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
  data <- read.table(poisson.txt,
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  with(data,
  plot(x, y, main = "Simulated Poisson Data",
       pch = 15, col = "blue"))
  model <- glm(y ~ x,
    data = data,
    family = "poisson"
  )
  print(model)
  print(summary(model))
  result <- regTermTest(model,
    formula(model),
    method = "Wald"
  )
  print(result)
  null_model <- glm(y ~ 1,
    data = data,
    family = "poisson"
  )
  print(lrtest(null_model, model))
  print(null_model$deviance - model$deviance)
  print(null_model)
  print(summary(null_model))
  print(anova(null_model, model, test = "Chisq"))
  print(anova(null_model, model, test = "Rao"))
  XBeta <- model.matrix(model) %*%
    coef(model)
  lambda <- exp(XBeta)
  data %<>%
    mutate(lambda = lambda) %>%
    mutate(prob = (exp(-lambda) * (lambda ^ y)) /
    factorial(y)) %>%
    mutate(fitted = model$fitted.values)
  print(data)
  with(data, {
  log_likelihood <- log(prod(prob))
  print(log_likelihood)
  log.lik.model <- logLik(model)
  print(log.lik.model)
  log.lik.nullmodel <- logLik(null_model)
  print(log.lik.nullmodel)
  GSQUARE <- 2 * (as.numeric(log.lik.model) -
                  as.numeric(log.lik.nullmodel))
  print(GSQUARE)
  print(1 - pchisq(GSQUARE, df = 1))
  pearson.statistic <-
    sum((y - lambda) ^ 2 / lambda)
  n <- nrow(data)
  p <- length(model$coefficients)
  print(1 - pchisq(pearson.statistic, df = n - p))
  terms <- ifelse(y == 0,
   0 - (y - lambda),
    y * log(y / lambda) -
    (y - lambda))
  deviance.statistic <-
    2 * sum(terms)
  print(1 - pchisq(deviance.statistic, df = n - p))
  pseudo.rsquare <- 1 -
    ((as.numeric(-2 * log.lik.model) /
     as.numeric(-2 * log.lik.nullmodel)))
  print(pseudo.rsquare)
  print(rsq(model, type = "n"))
  null.deviance <- model$null.deviance
  residual.deviance <- model$deviance
  rsquare <- 1 - (residual.deviance /
  null.deviance)
  print(rsquare)
  rsquared_vals <- lapply(c("v", "kl", "sse", "lr", "n"), rsquared, model)
  print(rsquared_vals)
    })
  return(0)
}

rsquared <- function(type, model) {
  rsquare <- rsq(model, type = type)
  longname <- revalue(type, c(
  "v" = "Variance function based",
  "kl" = "KL divergence based",
  "sse" = "SSE based",
  "lr" = "Likelihood Ratio Based",
  "n" = "Nagelkerke"))
  names(rsquare) <- longname
  return(rsquare)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

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
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(finalfit))
suppressPackageStartupMessages(library(oddsratio))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(ResourceSelection))

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
  print(sum(data$prob))
  log_likelihood <- log(prod(data$prob))
  print(log_likelihood)
  log.lik.model <- logLik(model)
  print(log.lik.model)
  log.lik.nullmodel <- logLik(null_model)
  print(log.lik.nullmodel)
  GSQUARE <- 2 * (as.numeric(log.lik.model) -
                  as.numeric(log.lik.nullmodel))
  print(GSQUARE)
  print(dchisq(x = GSQUARE, df = 1))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

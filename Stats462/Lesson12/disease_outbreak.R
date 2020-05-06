#!/usr/bin/env Rscript
disease.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/disease_outbreak.txt"
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
suppressPackageStartupMessages(library(glmulti))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(oddsratio))
suppressPackageStartupMessages(library(lmtest))

main <- function(argv) {
  data <- read.table(disease.txt,
    header = TRUE, as.is = TRUE
  )
  data %<>%
    select(-Case)
  print(head(data))
  print(skimr::skim(data))
  full_model <- glm(Disease ~ .^2,
    data = data,
    family = "binomial"
  )
  print(full_model)
  print(summary(full_model))
  glmulti.logistic.out <-
    glmulti(DF2formula(full_model$model),
      data = data,
      level = 2, # interactions considered
      method = "h", # Exhaustive approach
      crit = "bic", # BIC as criteria
      confsetsize = 1, # Keep 1 best models
      plotty = FALSE, report = FALSE, # No plot or interim reports
      fitfunction = "glm", # glm function
      family = binomial
    ) # binomial family for logistic regression
  print(glmulti.logistic.out@formulas)
  model <- glmulti.logistic.out@objects[[1]]
  print(summary(model))
  result <- regTermTest(model,
    formula(model),
    method = "Wald"
  )
  print(result)
  null_model <- glm(Disease ~ 1,
    data = data,
    family = "binomial"
  )
  print(lrtest(null_model, model))
  print(null_model$deviance - model$deviance)
  reduced_model <- glm(Disease ~ .,
    data = data,
    family = "binomial"
  )
  print(lrtest(reduced_model, full_model))
  print(reduced_model$deviance - full_model$deviance)
  print(anova(reduced_model, full_model))
  print(anova(reduced_model, full_model, test = "Rao"))
  print(anova(reduced_model, full_model, test = "LRT"))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

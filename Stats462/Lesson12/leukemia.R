#!/usr/bin/env Rscript
leukemia.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/leukemia_remission.txt"
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
library(finalfit)

main <- function(argv) {
  data <- read.table(leukemia.txt,
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  model <- glm(REMISS ~ .,
    data = data,
    family = "binomial"
  )
  print(model)
  print(summary(model))
  glmulti.logistic.out <-
    glmulti(DF2formula(data),
      data = data,
      level = 1, # No interaction considered
      method = "h", # Exhaustive approach
      crit = "bic", # BIC as criteria
      confsetsize = 1, # Keep 1 best models
      plotty = F, report = F, # No plot or interim reports
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
  #pred <- predict(model, se.fit = TRUE, type = "r")
  data %<>%
    mutate(fitted = model$fitted.values) %>%
    mutate(linear.fitted = model$linear.predictors) %>%
    arrange(LI)
  eqn <- logit_model_equation(model, digits = 4)
  with(data, {
         plot(LI, fitted, type = "b",
                  xlab = "LI", ylab = "probability of event", col = "red",
         ylim = c(-0.001, 1.001),
         main = paste0("Binary Fitted Line Plot\n",
                       eqn))
         points(LI, REMISS, pch = 15, col = "blue")
  })
  with(data, {
         plot(LI, exp(linear.fitted), type = "b",
                  xlab = "LI", ylab = "Odds", col = "red",
         main = "Odds Plot")
  })
  with(data, {
         plot(LI, linear.fitted, type = "b",
                  xlab = "LI", ylab = "Odds", col = "red",
         main = "Log Odds Plot")
  })

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

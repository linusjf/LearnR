#!/usr/bin/env Rscript
leukemia.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/leukemia_remission.txt"
  )
}, list())

library(skimr)
suppressPackageStartupMessages(library(survey))
suppressPackageStartupMessages(library(glmulti))

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

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

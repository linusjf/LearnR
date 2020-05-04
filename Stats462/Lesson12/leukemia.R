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

main <- function(argv) {
  data <- read.table(leukemia.txt,
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  model <- glm(REMISS ~ .,
               data = data,
               family = "binomial")
  print(model)
  print(summary(model))
  print(format(formula(model)))
  result <- regTermTest(model,
                        DF2formula(data),
                        method = "Wald")
  print(result)
  model <- glm(REMISS ~ LI,
               data = data,
               family = "binomial")
  print(model)
  print(summary(model))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

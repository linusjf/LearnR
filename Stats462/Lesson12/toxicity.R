#!/usr/bin/env Rscript
toxicity.txt <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/toxicity.txt")
}, list())

libfunc <- do.call(function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}, list())

library(skimr)
source(libfunc)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(survey))

main <- function(argv) {
  data <- read.table(toxicity.txt, header = TRUE, as.is = TRUE)
  data %<>%
    mutate(ObservedP = Deaths/SampSize)
  print(head(data))
  print(skimr::skim(data))
  reduced_model <- glm(ObservedP ~ Dose, data = data, family = "quasibinomial")
  print(reduced_model)
  print(summary(reduced_model))
  data %<>%
    mutate(fitted = reduced_model$fitted.values) %>%
    mutate(odds = fitted/(1 - fitted)) %>%
    mutate(odds.ratio = odds/lag(odds))
  print(head(data))
  result <- regTermTest(reduced_model, formula(reduced_model), method = "Wald")
  print(result)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

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
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

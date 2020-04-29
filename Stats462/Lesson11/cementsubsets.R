#!/usr/bin/env Rscript
cement.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/cement.txt"
  )
}

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(olsrr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  cement <- read.table(cement.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(cement))
  print(skimr::skim(cement))

  chart.Correlation(cement,
    histogram = TRUE,
    pch = 15
  )
  model <- lm(y ~ ., data = cement)
  best <- ols_step_best_subset(model)
  print(str(best))
  best_rsq <- best_model_rsquare(best)
  best_adjr <- best_model_adjrsquare(best)
  print(data.frame(best_rsq))
  print(data.frame(best_adjr))
  return(0)
}

best_model_rsquare <- function(models, rsqinc = 0.05) {
  models %<>%
    mutate(rsq.inc = ((rsquare / lag(rsquare) - 1)))
  models %<>%
    filter(rsq.inc >= rsqinc) %>%
    filter(n == max(n))
  return(models)
}

best_model_adjrsquare <- function(models) {
  models %<>%
    filter(adjr == max(adjr))
  return(models)
}


if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

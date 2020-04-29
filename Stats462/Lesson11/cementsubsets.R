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
  all <- ols_step_all_possible(model)
  print(str(all))
  print(
        head(
             data.frame(
                        best_model_rsquare(all))))
  return(0)
}

best_model_rsquare <- function(models, rsqinc = 0.05) {
  subset <- best_subset_rsquare(models)
  subset %<>%
    mutate(rsq.inc = ((rsquare / lag(rsquare) - 1)))
  subset %<>%
    filter(rsq.inc >= rsqinc) %>%
    filter(n == max(n))
  return(subset)
}

best_subset_rsquare <- function(models) {
  models %<>%
    group_by(n) %>%
    slice(which.max(rsquare)) %>%
    ungroup()
  return(models)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

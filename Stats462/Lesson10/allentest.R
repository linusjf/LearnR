#!/usr/bin/env Rscript
allentest.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/allentest.txt")
}

allentestn23.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/allentestn23.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(PerformanceAnalytics))

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(allentestn23.txt(), header = TRUE, as.is = TRUE)
  print(head(data))
  print(skimr::skim(data))

  data %<>%
    select(ACL, SDMT, Vocab, Abstract)
  scatterplot_matrix(data, "Scatterplots for allentestn23")
  chart.Correlation(data, histogram = TRUE, pch = 15)

  model <- lm(ACL ~ SDMT + Vocab + Abstract, data)
  print(anova(model))
  print(model_coeffs(model))
  print(model_fit_stats(model))

  data <- read.table(allentest.txt(), header = TRUE, as.is = TRUE)
  print(head(data))
  print(skimr::skim(data))

  data %<>%
    select(ACL, SDMT, Vocab, Abstract)
  scatterplot_matrix(data, "Scatterplots for allentestn23")
  chart.Correlation(data, histogram = TRUE, pch = 15)

  model <- lm(ACL ~ SDMT + Vocab + Abstract, data)
  print(anova(model))
  print(model_coeffs(model))
  print(model_fit_stats(model))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

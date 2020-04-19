#!/usr/bin/env Rscript
blaisdell.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/blaisdell.txt"
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
suppressPackageStartupMessages(library(lmtest))

main <- function(argv) {
  data <- read.table(blaisdell.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  model <- lm(comsales ~ indsales, data)

  print(model_coeffs(model))
  print(model_fit_stats(model))
  print(model_equation(model, digits = 4))
  print(dwtest(model))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

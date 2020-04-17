#!/usr/bin/env Rscript
realestate.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/realestate.txt"
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
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))

main <- function(argv) {
  data <- read.table(realestate.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  attach(data)
  model.1 <- lm(SalePrice ~ SqFeet + Lot, data)
  print(anova(model.1))
  print(model_fit_stats(model.1))
  print(model_coeffs(model.1))
  eqn.1 <- model_equation(model.1, digits = 4)
  print(eqn.1)

  detach(data)

  data %<>%
    mutate(lnSalePrice = log(SalePrice)) %>%
    mutate(lnSqFeet = log(SqFeet)) %>%
    mutate(lnLot = log(Lot))

  attach(data)
  model.log <- lm(lnSalePrice ~ lnSqFeet + lnLot, data)
  print(anova(model.log))
  print(model_fit_stats(model.log))
  print(model_coeffs(model.log))
  eqn.log <- model_equation(model.log, digits = 4)
  print(eqn.log)

  detach(data)

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

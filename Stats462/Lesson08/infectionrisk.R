#!/usr/bin/env Rscript
infectionrisk.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/infection_risk.txt"
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
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(e1071)

main <- function(argv) {
  data <- read.table(infectionrisk.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  data %<>%
    mutate(I2 = ifelse(Region == 2, 1, 0)) %>%
    mutate(I3 = ifelse(Region == 3, 1, 0)) %>%
    mutate(I4 = ifelse(Region == 4, 1, 0))
  
  simple <- fit_simple(data)

  return(0)
}

fit_simple <- function(data) {
  lm <- lm(InfctRsk ~ Stay + Xray + I2 + I3 + I4, data)
  summ <- summary(lm)
  anova <- anova(lm)
  print(summ)
  print(anova)
  return(lm)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(rsq))
suppressPackageStartupMessages(library(scatterplot3d))

main <- function(argv) {
  data <- read.table("../Data/infection_risk.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  reg <- lm(InfctRsk ~ Stay + Age + Xray, data = data)
  print(reg)
  print(anova(reg))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

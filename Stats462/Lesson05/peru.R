#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table("../Data/peru.txt",
    header = TRUE
  )
  data %<>%
    mutate(FracLife = Years / Age)
  print(head(data))
  print(skimr::skim(data))

  reg <- lm(Systol ~ . - Diastol - Age - Years, data = data)
  print(reg)
  fullanova <- anova(reg)
  print(fullanova)

  partreg <-
    lm(Systol ~ . - Height - Chin - Forearm - Calf - Pulse - Diastol - Age -
       Years,
       data = data)
  print(partreg)
  partanova <- anova(partreg)
  print(partanova)

  print(anova(partreg, reg))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

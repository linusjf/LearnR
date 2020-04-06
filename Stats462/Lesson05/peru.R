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

  reg <- lm(Systol ~ . - Diastol - FracLife, data = data)
  print(reg)
  print(summary(reg))
  fullanova <- anova(reg)
  print(fullanova)

  regratio <- lm(Systol ~ . - Diastol, data = data)
  print(regratio)
  print(summary(regratio))
  fullanovaratio <- anova(regratio)
  print(fullanovaratio)

  partreg <-
    lm(Systol ~ . - Height - Chin - Forearm - Calf - Pulse - Diastol,
      data = data
    )
  print(partreg)
  print(summary(partreg))
  partanova <- anova(partreg)
  print(partanova)

  print(anova(partreg, regratio))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

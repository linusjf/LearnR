#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table("../Data/peru.txt", header = TRUE)
  data %<>%
    mutate(logyears = log(Years)) %>%
    mutate(logage = log(Age))
  print(head(data))
  print(skimr::skim(data))

  reg <- lm(Systol ~ . - Diastol - logyears - logage, data = data)
  print(reg)
  print(summary(reg))
  fullanova <- anova(reg)
  print(fullanova)

  reglog <- lm(Systol ~ . - Diastol, data = data)
  print(reglog)
  print(summary(reglog))
  fullanovalog <- anova(reglog)
  print(fullanovalog)

  partreg <- lm(Systol ~ . - Height - Chin - Forearm - Calf - Pulse - Diastol,
    data = data
  )
  print(partreg)
  print(summary(partreg))
  partanova <- anova(partreg)
  print(partanova)

  print(anova(partreg, reglog))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

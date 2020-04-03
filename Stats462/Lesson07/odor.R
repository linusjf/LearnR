#!/usr/bin/env Rscript
library(scatterplot3d)
suppressPackageStartupMessages(library(e1071))

main <- function(argv) {
  data <- read.table("../Data/odor.txt",
    header = TRUE
  )

  print(head(data))
  print(skimr::skim(data))

  lm <- lm(Odor ~ Temp + Ratio + Height + I(Temp^2) +
           I(Ratio^2) + I(Height^2), data)
  summ <- summary(lm)

  print(summ)

  coefficients <- lm$coefficients
  intercept <- coefficients["(Intercept)"]
  tempcoeff <- coefficients["Temp"]
  tempsqcoeff <- coefficients["I(Temp^2)"]
  ratiocoeff <- coefficients["Ratio"]
  ratiosqcoeff <- coefficients["I(Ratio^2)"]
  heightcoeff <- coefficients["Height"]
  heightsqcoeff <- coefficients["I(Height^2)"]
  eqn <- paste(sprintf("%0.4f", intercept),
               sprintf("%+0.4f", tempcoeff),
               "Temp",
               sprintf("%+0.4f", ratiocoeff),
               "Ratio",
               sprintf("%+0.4f", heightcoeff),
               "Height",
               sprintf("%+0.4f", tempsqcoeff),
               "Temp^2",
               sprintf("%+0.4f", ratiosqcoeff),
               "Ratio^2",
               sprintf("%+0.4f", heightsqcoeff),
               "Height^2")

  print(eqn)

  lmreduced <- lm(Odor ~ Temp + Ratio + Height + I(Temp^2) +
           I(Ratio^2), data)
  summ <- summary(lmreduced)

  print(summ)

  coefficients <- lmreduced$coefficients
  intercept <- coefficients["(Intercept)"]
  tempcoeff <- coefficients["Temp"]
  tempsqcoeff <- coefficients["I(Temp^2)"]
  ratiocoeff <- coefficients["Ratio"]
  ratiosqcoeff <- coefficients["I(Ratio^2)"]
  heightcoeff <- coefficients["Height"]
  eqn <- paste(sprintf("%0.4f", intercept),
               sprintf("%+0.4f", tempcoeff),
               "Temp",
               sprintf("%+0.4f", ratiocoeff),
               "Ratio",
               sprintf("%+0.4f", heightcoeff),
               "Height",
               sprintf("%+0.4f", tempsqcoeff),
               "Temp^2",
               sprintf("%+0.4f", ratiosqcoeff),
               "Ratio^2")

  print(eqn)
  print(anova(lmreduced, lm))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

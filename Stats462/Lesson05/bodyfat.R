#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(rsq))
suppressPackageStartupMessages(library(scatterplot3d))

main <- function(argv) {
  data <- read.table("../Data/bodyfat.txt", header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  print(cor(data))
  print(cor.test(~ Bodyfat + Triceps, data))
  print(cor.test(~ Bodyfat + Thigh, data))
  print(cor.test(~ Bodyfat + Midarm, data))

  anova <- anova(lm(Bodyfat ~ Triceps + Thigh + Midarm, data = data))
  print(anova)
  mse <- anova$`Mean Sq`[4]
  X <- model.matrix(Bodyfat ~ Triceps + Thigh + Midarm, data = data)
  Xtranspose <- t(X)
  XtransposeX <- Xtranspose %*% X
  inverse <- solve(XtransposeX)
  sesquareb <- inverse * mse
  print(sesquareb)
  print(str(sesquareb))
  varb0 <- sesquareb[1, 1]
  seb0 <- sqrt(varb0)
  varb1 <- sesquareb[2, 2]
  print(varb1)
  seb1 <- sqrt(varb1)
  varb2 <- sesquareb[3, 3]
  seb2 <- sqrt(varb2)
  varb3 <- sesquareb[4, 4]
  seb3 <- sqrt(varb3)
  covb0b1 <- sesquareb[1, 2]
  covb1b2 <- sesquareb[2, 3]
  covb2b3 <- sesquareb[3, 4]
  corrb0b1 <- covb0b1 / (seb0 * seb1)
  corrb1b2 <- covb1b2 / (seb1 * seb2)
  corrb2b3 <- covb2b3 / (seb2 * seb3)
  values <- c(
    varb0, seb0, varb1, seb1, varb2, seb2, varb3, seb3, covb0b1, corrb0b1,
    covb1b2, corrb1b2, covb2b3, corrb2b3
  )
  names(values) <- c(
    "Var(b0)", "se(b0)", "Var(b1)", "se(b1)", "Var(b2)", "se(b2)",
    "Var(b3)", "se(b3)", "covariance(b0,b1)", "corr(b0,b1)", "covariance(b1,b2)",
    "corr(b1,b2)", "covariance(b2,b3)", "corr(b2,b3)"
  )
  print(values)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table("../Data/Physical.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  reg <- lm(Height ~ LeftArm + LeftFoot + HeadCirc + nose,
    data = data
  )
  print(reg)
  print(summary(reg))
  fullanova <- anova(reg)
  print(fullanova)
  plot(reg,
    which = 1,
    caption = "Residuals versus Fitted"
  )

  reducedreg <- lm(Height ~ LeftArm + LeftFoot,
    data = data
  )
  print(reducedreg)
  print(summary(reducedreg))
  reducedanova <- anova(reducedreg)
  print(reducedanova)

  print(anova(reducedreg, reg))

  leftarmreg <- lm(Height ~ LeftArm,
    data = data
  )
  print(leftarmreg)
  print(summary(leftarmreg))
  leftarmanova <- anova(leftarmreg)
  print(leftarmanova)

  sse_leftarm <- sum(leftarmreg$residuals**2)
  sse_reduced <- sum(reducedreg$residuals**2)
  Rsquaredleftarmgivenleftfoot <-
    (sse_leftarm - sse_reduced) / sse_leftarm
  print(Rsquaredleftarmgivenleftfoot)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

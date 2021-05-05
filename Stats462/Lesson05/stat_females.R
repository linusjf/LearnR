#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(rsq))
suppressPackageStartupMessages(library(scatterplot3d))

main <- function(argv) {
  data <- read.table("../Data/stat_females.txt", header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  print(cor(data))
  print(cor.test(~momheight + dadheight, data))
  print(cor.test(~momheight + Height, data))
  print(cor.test(~dadheight + Height, data))


  reg <- lm(Height ~ momheight, data = data)
  print(reg)
  print(anova(reg))

  reg <- lm(Height ~ dadheight, data = data)
  print(reg)
  print(anova(reg))

  reg <- lm(Height ~ momheight + dadheight, data = data)
  print(reg)
  print(anova(reg))

  scatterplot3d(data, main = "Height versus momheight,dadheight", axis = TRUE, 
    grid = TRUE, highlight.3d = TRUE, type = "p", angle = 45)

  plot(reg)

  residuals <- resid(reg)
  plot(data$momheight, residuals, xlab = "Mom's height", ylab = "Residuals", main = "Mom's height versus Residuals")
  plot(data$dadheight, residuals, xlab = "Dad's height", ylab = "Residuals", main = "Dad's height versus Residuals")

  reg <- lm(dadheight ~ momheight, data = data)
  print(reg)
  print(anova(reg))
  reg <- lm(dadheight ~ momheight - 1, data = data)
  print(reg)
  print(anova(reg))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))

main <- function(argv) {
  data <- read.table("../Data/iqsize.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  reg <- lm(PIQ ~ Brain + Height, data = data)
  plot(reg, which = 1,
  caption = "Residuals versus Fitted")

  residuals <- resid(reg)
  plot(data$Brain, residuals, xlab = "Brain", main = "Residuals versus Brain")
  abline(h = mean(residuals))
  plot(data$Height,
       residuals,
       xlab = "Height",
       main = "Residuals versus Height")
  abline(h = mean(residuals))
  hist(residuals)
  plot(density(residuals))
  # A quantile normal plot - good for checking normality
  qqnorm(residuals)
  qqline(residuals)
  normal_sample <- rnorm(length(residuals),
  0, sd(residuals))
  q1 <- quantile(residuals,
                  probs = seq(0, 1, 1 / length(residuals)))
  q2 <- quantile(normal_sample,
                  probs = seq(0, 1, 1 / length(residuals)))
  plot(q1, q2, xlab = "Residual Quantiles",
  ylab = "Theoretical Quantiles",
  main = "QQ plot")
  abline(lm(q2 ~ q1))
  plot(data$Weight,
       residuals,
       xlab = "Weight",
       main = "Residuals versus Weight")
  abline(h = mean(residuals))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(nortest))
suppressPackageStartupMessages(library(e1071))

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
  abline(a = 0, b = 1)
  plot(data$Weight,
       residuals,
       xlab = "Weight",
       main = "Residuals versus Weight")
  abline(h = mean(residuals))

  probplot(residuals,
           probs = c(0.10, 0.25, 0.5, 0.75, 0.9, 0.99),
  xlab = "Residuals",
  ylab = "Probabilities (Percent)")

  ad <- ad.test(residuals)
  print(ad)
  labels <- c(paste0("Mean: ", round(mean(residuals), 4)),
  paste0("Stdev: ", round(sd(residuals), 2)),
  paste0("Count: ", round(length(residuals), 2)),
  paste0("AD: ", round(ad$statistic, 4)),
  paste0("p-value: ", round(ad$p.value, 4)))
  legend("bottomright", legend = labels)

  shapiro <- shapiro.test(residuals)
  print(shapiro)
  
  ks <- ks.test(residuals, "pnorm", mean(residuals),
  sd(residuals))
  print(ks)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

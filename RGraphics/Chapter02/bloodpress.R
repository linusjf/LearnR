#!/usr/bin/env Rscript

main <- function(argv) {
  data <- read.table("bloodpress.txt",
    sep = "\t",
    header = TRUE
  )
  print(head(data))
  print(summary(data))
  age_regression <- lm(BP ~ Age, data = data)
  print(summary(age_regression))
  plot(data$Age, data$BP, main = "Regression plot",
     xlab = "Age", ylab = "Blood pressure",
     pch = 19, frame = FALSE)
  abline(age_regression, col = "blue")
  plot(data$Age, resid(age_regression),
       xlab = "Age", ylab = "Residuals")
  abline(h = 0, col = "blue")
  plot(age_regression)
  weight_regression <- lm(BP ~ Weight, data = data)
  print(summary(weight_regression))
  plot(data$Weight, data$BP, main = "Regression plot",
     xlab = "Weight", ylab = "Blood pressure",
     pch = 19, frame = FALSE)
  abline(weight_regression, col = "blue")
  plot(data$Weight, resid(weight_regression),
     xlab = "Weight", ylab = "Residuals")
  abline(h = 0, col = "blue")
  plot(weight_regression)
  duration_regression <- lm(BP ~ Dur, data = data)
  print(summary(duration_regression))
  plot(data$Dur, data$BP, main = "Regression plot",
     xlab = "Duration", ylab = "Blood pressure",
     pch = 19, frame = FALSE)
  abline(duration_regression, col = "blue")
  plot(data$Dur,
       resid(duration_regression),
     xlab = "Duration", ylab = "Residuals")
  abline(h = 0, col = "blue")
  plot(duration_regression)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

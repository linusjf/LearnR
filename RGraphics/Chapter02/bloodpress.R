#!/usr/bin/env Rscript

main <- function(argv) {
  par(mar = c(6, 4, 6, 2), mex = 0.8)
  data <- read.table("bloodpress.txt",
    sep = "\t",
    header = TRUE
  )
  print(head(data))
  print(summary(data))
  age_regression <- lm(BP ~ Age, data = data)
  coefs <- coefficients(age_regression)
  print(anova(age_regression))
  main_label <-
    paste0("Regression plot\nBP = ",
           coefs["(Intercept)"],
           " + ",
           coefs["Age"],
           " Age\n",
    "S = ", sigma(age_regression),
    "\nR-sq = ", summary(age_regression)$r.squared,
    " R-sq(adj) = ", summary(age_regression)$adj.r.squared)
  plot(data$Age, data$BP, main = main_label,
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

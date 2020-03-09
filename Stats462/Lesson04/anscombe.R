#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/anscombe.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_anscombe(data)

  return(0)
}

process_anscombe <- function(data) {
  reg <- lm(y3 ~ x3, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_anscombe(data, reg)
  plot_fitted(reg)
  plot_predictor(data, reg)
  plot_stdresid(reg)
  plot_stdresid_predictor(data, reg)
  residuals <- resid(reg)
  std_residuals <- residuals / sd(residuals)
  indexes <- unname(which(std_residuals <= 2 & std_residuals >= -2))
  data <- data[c(indexes), ]
  reg <- lm(y3 ~ x3, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_anscombe(data, reg)
  plot_fitted(reg)
  plot_predictor(data, reg)
  plot_stdresid(reg)
  plot_stdresid_predictor(data, reg)
}

plot_stdresid_predictor <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  std_residuals <- residuals / sd(residuals)
  main_label <- "Predictor versus standardized residuals"
  plot(data$x3, std_residuals,
    main = main_label,
    xlab = "x3", ylab = "Standardized Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(std_residuals), col = "black", lty = "dashed")
}

plot_stdresid <- function(reg) {
  par(mar = c(4, 4, 4, 1))
  predicted <- predict(reg)
  residuals <- resid(reg)
  std_residuals <- residuals / sd(residuals)
  main_label <- "Fitted values versus standardized residuals"
  plot(predicted, std_residuals,
    main = main_label,
    xlab = "Fitted value", ylab = "Standardized Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(std_residuals), col = "black", lty = "dashed")
}

plot_fitted <- function(reg) {
  par(mar = c(4, 4, 4, 1))
  predicted <- predict(reg)
  residuals <- resid(reg)
  main_label <- "Fitted values versus residuals"
  plot(predicted, residuals,
    main = main_label,
    xlab = "Fitted value", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}


plot_predictor <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  main_label <- "Predictor versus residuals"
  plot(data$x3, residuals,
    main = main_label,
    xlab = "x3", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_anscombe <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "x3 versus y3\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["x3"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["x3"]), "x3"
  )
  plot(data$x3, data$y3,
    main = main_label,
    xlab = "x3", ylab = "y3",
    pch = 19, frame = TRUE
  )
  abline(reg, col = "blue")
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("topleft", legends)
  abline(h = mean(data$y3), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

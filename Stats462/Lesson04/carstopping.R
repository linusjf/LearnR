#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/carstopping.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_carstopping(data)

  return(0)
}

process_carstopping <- function(data) {
  reg <- lm(StopDist ~ Speed, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_carstopping(data, reg)
  plot_fitted(reg)
  plot_predictor(data, reg)
  plot_stdresid(reg)
  plot_stdresid_predictor(data, reg)
  hist(resid(reg))
  plot(reg,
    which = c(2),
    caption = list("Normal Q-Q"),
    qqline = TRUE
  )
  reg <- lm(sqrt(StopDist) ~ Speed, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_sqrtcarstopping(data, reg)
  plot_fitted(reg)
  plot_predictor(data, reg)
  plot_stdresid(reg)
  plot_stdresid_predictor(data, reg)
  hist(resid(reg))
  plot(reg,
    which = c(2),
    caption = list("Normal Q-Q"),
    main = "Q-Q plot",
    qqline = TRUE
  )
}

plot_stdresid_predictor <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  std_residuals <- residuals / sd(residuals)
  main_label <- "Predictor versus standardized residuals"
  plot(data$Speed, std_residuals,
    main = main_label,
    xlab = "Speed", ylab = "Standardized Residuals",
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
  plot(data$Speed, residuals,
    main = main_label,
    xlab = "Speed", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_sqrtcarstopping <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Speed versus Sqrt(StopDist)\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["Speed"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["Speed"]), "Speed"
  )
  plot(data$Speed, sqrt(data$StopDist),
    main = main_label,
    xlab = "Speed", ylab = "Sqrt(StopDist)",
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
  abline(h = mean(sqrt(data$StopDist)), col = "black", lty = "dashed")
}

plot_carstopping <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Speed versus StopDist\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["Speed"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["Speed"]), "Speed"
  )
  plot(data$Speed, data$StopDist,
    main = main_label,
    xlab = "Speed", ylab = "StopDist",
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
  abline(h = mean(data$StopDist), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/realestate.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_realestate(data)

  return(0)
}

process_realestate <- function(data) {
  reg <- lm(SalePrice ~ SqFeet, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_realestate(data, reg)
  plot_fitted(reg)
  plot_predictor(data, reg)
  plot_stdresid(reg)
  plot_stdresid_predictor(data, reg)
  hist(resid(reg))
  plot(reg, which = c(2),
       caption = list("Normal Q-Q"),
       qqline = TRUE)
}

plot_stdresid_predictor <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  std_residuals <- residuals / sd(residuals)
  main_label <- "Predictor versus standardized residuals"
  plot(data$SqFeet, std_residuals,
    main = main_label,
    xlab = "SqFeet", ylab = "Standardized Residuals",
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
  plot(data$SqFeet, residuals,
    main = main_label,
    xlab = "x", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_realestate <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Square Feet versus Sale Price\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["SqFeet"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["SqFeet"]), "SqFeet"
  )
  plot(data$SqFeet, data$SalePrice,
    main = main_label,
    xlab = "SqFeet", ylab = "SalePrice",
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
  abline(h = mean(data$SqFeet), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

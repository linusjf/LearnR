#!/usr/bin/env Rscript
alcoholtobacco.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/alcoholtobacco.txt"
  )
}
library(skimr)

main <- function(argv) {
  data <- read.table(alcoholtobacco.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_alcoholtobacco(data)

  return(0)
}

process_alcoholtobacco <- function(data) {
  reg <- lm(Alcohol ~ Tobacco, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_alcoholtobacco(data, reg)
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
  residuals <- resid(reg)
  std_residuals <- residuals / sd(residuals)
  indexes <- unname(which(std_residuals <= 2 & std_residuals >= -2))
  data <- data[c(indexes), ]
  reg <- lm(Alcohol ~ Tobacco, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_alcoholtobacco(data, reg)
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
  plot(data$Tobacco, std_residuals,
    main = main_label,
    xlab = "Tobacco", ylab = "Standardized Residuals",
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
  plot(data$Tobacco, residuals,
    main = main_label,
    xlab = "Tobacco", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_alcoholtobacco <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Alcohol versus Tobacco\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["Tobacco"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["Tobacco"]), "Tobacco"
  )
  plot(data$Tobacco, data$Alcohol,
    main = main_label,
    xlab = "Tobacco", ylab = "Alcohol",
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
  abline(h = mean(data$Alcohol), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

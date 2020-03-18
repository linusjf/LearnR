#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/infection_risk.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_infection_risk(data)

  return(0)
}

process_infection_risk <- function(data) {
  reg <- lm(InfctRsk ~ Stay, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  ci <- predict(
                reg,
                interval = "confidence")
  predi <- predict(
                reg,
                interval = "prediction")
  plot_infection_risk(data, reg, ci, predi)
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
  plot(data$Stay, std_residuals,
    main = main_label,
    xlab = "Stay", ylab = "Standardized Residuals",
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
  plot(data$Stay, residuals,
    main = main_label,
    xlab = "Stay", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_infection_risk <- function(data, reg, ci, predi) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Stay versus Infection Risk\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["Stay"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["Stay"]), "Stay"
  )
  plot(data$Stay, data$InfctRsk,
    main = main_label,
    xlab = "Stay", ylab = "Infection Risk",
    pch = 19, frame = TRUE
  )
  abline(reg, col = "blue")
matlines(data$Stay, ci[, c("lwr", "upr")],
         col = "red", lty = 1, type = "l")
matlines(data$Stay, predi[, c("lwr", "upr")],
         col = "blue", lty = 1, type = "l")
polygon(c(data$Stay, rev(data$Stay)),
        c(predi[, "upr"],
          rev(predi[, "lwr"])),
        col = adjustcolor("orangered",
                          alpha.f = 0.7), border = NA)
polygon(c(data$Stay, rev(data$Stay)),
        c(ci[, "upr"],
          rev(ci[, "lwr"])),
        col = adjustcolor("wheat",
                          alpha.f = 0.7), border = NA)
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("bottomright", legends)
  abline(h = mean(data$InfctRsk), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

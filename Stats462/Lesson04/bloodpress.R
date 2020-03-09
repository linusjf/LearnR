#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/bloodpress.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  process_bloodpress(data)
  return(0)
}

process_bloodpress <- function(data) {
  process_age(data)
  process_weight(data)
  process_duration(data)
  process_age_weight(data)
}

process_duration <- function(data) {
  reg <- lm(BP ~ Dur, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_duration(data, reg)
  plot_durationfitted(reg)
  plot_durationpredictor(data, reg)
}

process_weight <- function(data) {
  reg <- lm(BP ~ Weight, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_weight(data, reg)
  plot_weightfitted(reg)
  plot_weightpredictor(data, reg)
}

process_age_weight <- function(data) {
  reg <- lm(BP ~ Age + Weight, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_duration_vs_residuals(data, reg)
}

process_age <- function(data) {
  reg <- lm(BP ~ Age, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_age(data, reg)
  plot_agefitted(reg)
  plot_agepredictor(data, reg)
  plot_weight_vs_residuals(data, reg)
  hist(resid(reg))
}

plot_duration_vs_residuals <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  main_label <- "Duration versus Age+Weight residuals"
  plot(data$Dur, residuals,
    main = main_label,
    xlab = "Dur", ylab = "Age+Weight Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
  res_reg <- lm(residuals ~ data$Dur)
  abline(res_reg, col = "blue")
  summ <- summary(res_reg)
  print(summ)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4)),
    paste0("p-value - "),
    format(summ$coefficients["data$Dur", "Pr(>|t|)"],
      digits = 2, nsmall = 2, scientific = TRUE
    )
  )
  legend("topleft", legends)
}

plot_weight_vs_residuals <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  main_label <- "Weight versus Age residuals"
  plot(data$Weight, residuals,
    main = main_label,
    xlab = "Weight", ylab = "Age Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
  res_reg <- lm(residuals ~ data$Weight)
  abline(res_reg, col = "blue")
  summ <- summary(res_reg)
  print(summ)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4)),
    paste0("p-value - "),
    format(summ$coefficients["data$Weight", "Pr(>|t|)"],
      digits = 2, nsmall = 2, scientific = TRUE
    )
  )
  legend("bottomright", legends)
}

plot_agefitted <- function(reg) {
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

plot_weightfitted <- function(reg) {
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

plot_durationfitted <- function(reg) {
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

plot_agepredictor <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  main_label <- "Predictor versus residuals"
  plot(data$Age, residuals,
    main = main_label,
    xlab = "Age", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_durationpredictor <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  main_label <- "Predictor versus residuals"
  plot(data$Dur, residuals,
    main = main_label,
    xlab = "Duration", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_weightpredictor <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  residuals <- resid(reg)
  main_label <- "Predictor versus residuals"
  plot(data$Weight, residuals,
    main = main_label,
    xlab = "Weight", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_age <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Age versus Blood Pressure\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["Age"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["Age"]), "Age"
  )
  plot(data$Age, data$BP,
    main = main_label,
    xlab = "Age", ylab = "Blood Pressure",
    pch = 19, frame = TRUE
  )
  abline(reg, col = "blue")
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("bottomright", legends)
  abline(h = mean(data$BP), col = "black", lty = "dashed")
}

plot_duration <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Duration versus Blood Pressure\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["Dur"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["Dur"]), "Duration"
  )
  plot(data$Dur, data$BP,
    main = main_label,
    xlab = "Duration", ylab = "Blood Pressure",
    pch = 19, frame = TRUE
  )
  abline(reg, col = "blue")
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("bottomright", legends)
  abline(h = mean(data$BP), col = "black", lty = "dashed")
}

plot_weight <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Weight versus Blood Pressure\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["Weight"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["Weight"]), "Weight"
  )
  plot(data$Weight, data$BP,
    main = main_label,
    xlab = "Weight", ylab = "Blood Pressure",
    pch = 19, frame = TRUE
  )
  abline(reg, col = "blue")
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("bottomright", legends)
  abline(h = mean(data$BP), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

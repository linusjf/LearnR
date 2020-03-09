#!/usr/bin/env Rscript
library(skimr)
library(lawstat)

main <- function(argv) {
  data <- read.table("../Data/alphapluto.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_alphapluto(data)

  return(0)
}

process_alphapluto <- function(data) {
  reg <- lm(alpha ~ pluto, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  print(levene.test(data[["alpha"]],
            data[["pluto"]],
            location = "median",
            correction.method = "zero.correction"))
  plot_alphapluto(data, reg)
  plot_fitted(reg)
  plot_predictor(data, reg)
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
  plot(data$pluto, residuals,
    main = main_label,
    xlab = "pluto", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_alphapluto <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Plutonium activity versus Alpha Count Rate\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["pluto"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["pluto"]), "pluto"
  )
  plot(data$pluto, data$alpha,
    main = main_label,
    xlab = "pluto", ylab = "alpha",
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
  abline(h = mean(data$alpha), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

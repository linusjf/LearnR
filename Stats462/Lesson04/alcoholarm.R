#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/alcoholarm.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_alcoholarm(data)

  return(0)
}

process_alcoholarm <- function(data) {
  reg <- lm(strength ~ alcohol, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_alcoholarm(data, reg)
  plot_fitted(reg)
  plot_predictor(data, reg)
  hist(resid(reg))
  plot(reg, which = c(2),
       caption = list("Normal Q-Q"),
       main = "Q-Q plot",
       qqline = TRUE)
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
  plot(data$alcohol, residuals,
    main = main_label,
    xlab = "Alcohol", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_alcoholarm <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Lifetime Alcohol Consumption versus Deltoid Strength\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["alcohol"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["alcohol"]), "alcohol"
  )
  plot(data$alcohol, data$strength,
    main = main_label,
    xlab = "Alcohol", ylab = "Strength",
    pch = 19, frame = TRUE
  )
  abline(reg, col = "blue")
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("topright", legends)
  abline(h = mean(data$strength), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

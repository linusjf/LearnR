#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/handsheight.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_handsheight(data)

  return(0)
}

process_handsheight <- function(data) {
  reg <- lm(HandSpan ~ Height, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_handsheight(data, reg)
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
  plot(data$Height, residuals,
    main = main_label,
    xlab = "Height", ylab = "Residuals",
    pch = 19, frame = TRUE
  )
  abline(h = mean(residuals), col = "black", lty = "dashed")
}

plot_handsheight <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Height versus Handspan\n",
    coefs["(Intercept)"],
    x <- if (sign(coefs["Height"]) == 1) {
      "+"
    } else {
      "-"
    },
    abs(coefs["Height"]), "Height"
  )
  plot(data$Height, data$HandSpan,
    main = main_label,
    xlab = "Height", ylab = "HandSpan",
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
  abline(h = mean(data$HandSpan), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
poverty <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/poverty.txt")
}
library(skimr)
library(Metrics)
suppressMessages(library(dvmisc))

main <- function(argv) {
  data <- read.table(poverty(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Brth15to17 ~ PovPct, data = data)
  print(summary(reg))
  predicted <- predict(reg)
  actuals <- data$Brth15to17
  print(Metrics::rmse(actuals, predicted))
  print(sqrt(dvmisc::get_mse(reg, var.estimate = FALSE)))

  # Another way to compute R^2

  # nolint start
  SST <- sum((data$Brth15to17 - mean(data$Brth15to17))^2)
  print(paste("Total Sum of Squares: ", SST))
  SSR <- sum((predicted - mean(data$Brth15to17))^2)
  print(paste("Squared Sum of Residuals: ", SSR))
  SSE <- sum(resid(reg)^2)
  print(paste("Squared Sum of Errors: ", SSE))
  print(c(SSR/SST, summary(reg)$r.squared))
  # nolint end

  print(cor(data$PovPct, data$Brth15to17))
  plot_poverty(data, reg)
  return(0)
}

plot_poverty <- function(data, reg) {
  par(mar = c(4, 4, 4, 1))
  coefs <- reg$coefficients
  main_label <- paste("PovPct versus Brth15to17\n", coefs["(Intercept)"], " + ", 
    coefs["PovPct"], "PovPct")
  plot(data$PovPct, data$Brth15to17, main = main_label, xlab = "PovPct", ylab = "Brth15to17", 
    pch = 19, frame = FALSE)
  abline(reg, col = "blue")
  box(which = "plot", lty = "solid")
  summ <- summary(reg)
  legends <- c(paste("S", format(summ$sigma, digits = 6)), paste("R-squared", format(summ$r.squared, 
    digits = 6)))
  legend("top", inset = c(0, 0), legends, horiz = TRUE, col = rainbow(3), xpd = TRUE, 
    x.intersp = 0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
library(skimr)
library(Metrics)
suppressMessages(library(dvmisc))

main <- function(argv) {
  data <- read.table("../Data/heightgpa.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(gpa ~ height, data = data)
  print(summary(reg))
  new_data <- data.frame(
    height = c(66, 67, 68, 69, 70, 71)
  )
  values <- predict(reg, newdata = new_data)
  print(diff(values))
  predicted <- predict(reg)
  actuals <- data$gpa
  print(Metrics::rmse(actuals, predicted))
  print(sqrt(dvmisc::get_mse(reg, var.estimate = FALSE)))

  # Another way to compute R^2

  # nolint start
  SST <- sum((data$gpa - mean(data$gpa))^2)
  print(paste("Total Sum of Squares: ", SST))
  SSR <- sum((predicted - mean(data$gpa))^2)
  print(paste("Squared Sum of Residuals: ", SSR))
  SSE <- sum(resid(reg)^2)
  print(paste("Squared Sum of Errors: ", SSE))
  print(c(SSR / SST, summary(reg)$r.squared))
  # nolint end

  print(cor(data$gpa, data$height))
  plot_heightgpa(data, reg)
  return(0)
}

plot_heightgpa <- function(data, reg) {
  par(mar = c(4, 7, 4, 1))
  plot(data$height, data$gpa,
    main = "Height versus gpa",
    xlab = "Height", ylab = "GPA",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  x0 <- mean(data$height)
  y0 <- mean(data$gpa)
  x1 <- x0 + 2
  y1 <- y0 + .5
  arrows(x1, y1, x0, y0,
    angle = 30, code = 2, col = "black", lwd = 4
  )
  coefs <- reg$coefficients
  text(x1, y1 + 0.1, paste(
    coefs["(Intercept)"],
    "\n",
    coefs["height"], "height"
  ))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

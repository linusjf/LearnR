#!/usr/bin/env Rscript
signdist <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/signdist.txt")
}
library(skimr)
library(Metrics)
suppressMessages(library(dvmisc))

main <- function(argv) {
  data <- read.table(signdist(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Distance ~ Age, data = data)
  print(summary(reg))
  new_data <- data.frame(Age = c(46, 47, 48, 49, 50, 51))
  values <- predict(reg, newdata = new_data)
  print(diff(values))
  predicted <- predict(reg)
  actuals <- data$Distance
  print(Metrics::rmse(actuals, predicted))
  print(sqrt(dvmisc::get_mse(reg, var.estimate = FALSE)))

  # Another way to compute R^2

  # nolint start
  SST <- sum((data$Distance - mean(data$Distance))^2)
  print(paste("Total Sum of Squares: ", SST))
  SSR <- sum((predicted - mean(data$Distance))^2)
  print(paste("Squared Sum of Residuals: ", SSR))
  SSE <- sum(resid(reg)^2)
  print(paste("Squared Sum of Errors: ", SSE))
  print(c(SSR / SST, summary(reg)$r.squared))
  # nolint end

  print(cor(data$Age, data$Distance))
  plot_signdist(data, reg)
  return(0)
}

plot_signdist <- function(data, reg) {
  par(mar = c(4, 7, 4, 1))
  plot(data$Age, data$Distance,
    main = "Age versus distance", xlab = "Age", ylab = "Distance",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  x0 <- mean(data$Age)
  y0 <- mean(data$Distance)
  x1 <- x0 + 20
  y1 <- y0 + 100
  arrows(x1, y1, x0, y0, angle = 30, code = 2, col = "black", lwd = 4)
  coefs <- reg$coefficients
  text(x1, y1 + 20, paste(coefs["(Intercept)"], "\n", coefs["Age"], "Age"))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

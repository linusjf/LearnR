#!/usr/bin/env Rscript
student_height_weight() <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/student_height_weight.txt")
}
library(skimr)
library(Metrics)
suppressMessages(library(dvmisc))

main <- function(argv) {
  data <- read.table(student_height_weight(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(wt ~ ht, data = data)
  print(summary(reg))
  new_data <- data.frame(ht = c(66, 67, 68, 69, 70, 71))
  values <- predict(reg, newdata = new_data)
  print(diff(values))
  predicted <- predict(reg)
  actuals <- data$wt
  print(Metrics::rmse(actuals, predicted))
  print(sqrt(dvmisc::get_mse(reg, var.estimate = FALSE)))

  # Another way to compute R^2

  # nolint start
  SST <- sum((data$wt - mean(data$wt))^2)
  print(paste("Total Sum of Squares: ", SST))
  SSR <- sum((predicted - mean(data$wt))^2)
  print(paste("Squared Sum of Residuals: ", SSR))
  SSE <- sum(resid(reg)^2)
  print(paste("Squared Sum of Errors: ", SSE))
  print(c(SSR/SST, summary(reg)$r.squared))
  # nolint end

  print(cor(data$ht, data$wt))
  plot_student(data, reg)
  return(0)
}

plot_student <- function(data, reg) {
  par(mar = c(4, 7, 4, 1))
  plot(data$ht, data$wt, main = "Height versus weight", xlab = "height", ylab = "weight", 
    pch = 19, frame = FALSE)
  abline(reg, col = "blue")
  x0 <- mean(data$ht)
  y0 <- mean(data$wt)
  x1 <- x0 + 2
  y1 <- y0 + 25
  arrows(x1, y1, x0, y0, angle = 30, code = 2, col = "black", lwd = 4)
  coefs <- reg$coefficients
  text(x1, y1 + 3, paste(coefs["(Intercept)"], " + \n", coefs["ht"], "ht"))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

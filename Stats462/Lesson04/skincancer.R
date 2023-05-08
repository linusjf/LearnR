#!/usr/bin/env Rscript
skincancer.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/skincancer.txt")
}
library(skimr)

main <- function(argv) {
  data <- read.table(skincancer.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Mort ~ Lat, data = data)
  print(reg)
  print(summary(reg))
  print("Mean of Lat")
  print(mean(data$Lat))
  newdata <- data.frame(Lat = c(40, 28, 150))
  print("New Data:")
  print(newdata)
  print("Confidence Interval")
  ci <- predict(reg, newdata, interval = "confidence")
  print(ci)
  print("Prediction Interval")
  predi <- predict(reg, newdata, interval = "prediction")
  print(predi)
  ci <- predict(reg, interval = "confidence")
  predi <- predict(reg, interval = "prediction")
  plot_skin_cancer(data, reg, ci, predi)
  return(0)
}

plot_skin_cancer <- function(data, reg, ci, predi) {
  par(mar = c(4, 7, 4, 1))
  plot(data$Lat, data$Mort,
    main = "Skin cancer mortality versus state latitude",
    xlab = "Latitude (at centre of state)", ylab = "Mortality (Deaths per 10
     million)",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  matlines(data$Lat, ci[, c("lwr", "upr")], col = "red", lty = 1, type = "l")
  matlines(data$Lat, predi[, c("lwr", "upr")], col = "blue", lty = 1, type = "l")
  polygon(c(data$Lat, rev(data$Lat)), c(predi[, "upr"], rev(predi[, "lwr"])), col = adjustcolor("orangered",
    alpha.f = 0.7
  ), border = NA)
  polygon(c(data$Lat, rev(data$Lat)), c(ci[, "upr"], rev(ci[, "lwr"])), col = adjustcolor("wheat",
    alpha.f = 0.7
  ), border = NA)
  x0 <- mean(data$Lat)
  y0 <- mean(data$Mort)
  x1 <- x0 + 5
  y1 <- y0 + 40
  arrows(x1, y1, x0, y0, angle = 30, code = 2, col = "black", lwd = 4)
  coefs <- reg$coefficients
  text(x1, y1 + 10, paste(coefs["(Intercept)"], "\n", coefs["Lat"], "Lat"))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

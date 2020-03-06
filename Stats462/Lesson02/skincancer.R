#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/skincancer.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Mort ~ Lat, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_skin_cancer(data, reg)
  return(0)
}

plot_skin_cancer <- function(data, reg) {
  par(mar = c(4, 7, 4, 1))
  plot(data$Lat, data$Mort,
    main = "Skin cancer mortality versus state latitude",
    xlab = "Latitude (at centre of state)", ylab = "Mortality (Deaths per 10
     million)",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  x0 <- mean(data$Lat)
  y0 <- mean(data$Mort)
  x1 <- x0 + 5
  y1 <- y0 + 40
  arrows(x1, y1, x0, y0,
    angle = 30, code = 2, col = "black", lwd = 4
  )
  coefs <- reg$coefficients
  text(x1, y1 + 10, paste(
    coefs["(Intercept)"],
    "\n",
    coefs["Lat"], "Lat"
  ))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

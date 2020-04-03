#!/usr/bin/env Rscript
library(scatterplot3d)
suppressPackageStartupMessages(library(e1071))

main <- function(argv) {
  data <- read.table("../Data/yield.txt",
    header = TRUE
  )

  print(head(data))
  print(skimr::skim(data))

  lm <- lm(Yield ~ Temp, data)
  summ <- summary(lm)
  sigma <- sigma(lm)
  r_squared <- summ$r.squared
  adj_r_squared <- summ$adj.r.squared

  coefficients <- lm$coefficients
  intercept <- coefficients["(Intercept)"]
  tempcoeff <- coefficients["Temp"]
  eqn <- paste(sprintf("%0.4f", intercept),
               sprintf("%+0.4f", tempcoeff),
               "Temp")
  labels <- c(paste("Sigma: ", sprintf("%+0.4f", sigma)),
  paste("R Squared: ", sprintf("%+0.4f", r_squared)),
  paste("Adj R Squared: ", sprintf("%+0.4f", adj_r_squared)))

  # scatter plot
   plot(data$Temp, data$Yield,
    type = "p",
    pch = 16,
    main = "Fitted Line Plot",
    sub = eqn,
    xlab = "Temp",
    ylab = "Yield"
  )
  print("Curve....")
  curve(intercept + tempcoeff * x,
        add = TRUE,
  col = "red")
  legend("bottomright", legend = labels)

  lm <- lm(Yield ~ Temp + I(Temp^2), data)
  summ <- summary(lm)
  sigma <- sigma(lm)
  r_squared <- summ$r.squared
  adj_r_squared <- summ$adj.r.squared

  coefficients <- lm$coefficients
  print(coefficients)
  intercept <- coefficients["(Intercept)"]
  tempcoeff <- coefficients["Temp"]
  tempsqcoeff <- coefficients["I(Temp^2)"]
  eqn <- paste(sprintf("%0.4f", intercept),
               sprintf("%+0.4f", tempcoeff),
               "Temp",
               sprintf("%+0.4f", tempsqcoeff),
               "Temp^2")
  labels <- c(paste("Sigma: ", sprintf("%+0.4f", sigma)),
  paste("R Squared: ", sprintf("%+0.4f", r_squared)),
  paste("Adj R Squared: ", sprintf("%+0.4f", adj_r_squared)))
  # scatter plot
   plot(data$Temp, data$Yield,
    type = "p",
    pch = 16,
    main = "Fitted Line Plot",
    sub = eqn,
    xlab = "Temp",
    ylab = "Yield"
  )
  print("Curve....")
  curve(intercept + tempcoeff * x + tempsqcoeff * (x^2),
        add = TRUE,
  col = "red")

  legend("bottomright", legend = labels)
  print(anova(lm))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

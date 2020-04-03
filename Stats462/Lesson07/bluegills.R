#!/usr/bin/env Rscript
library(scatterplot3d)
suppressPackageStartupMessages(library(e1071))

main <- function(argv) {
  data <- read.table("../Data/bluegills.txt",
    header = TRUE
  )

  print(head(data))
  print(skimr::skim(data))
  # scatter plot
   plot(data,
    type = "p",
    pch = 16,
    main = "Scatterplot Age versus Length",
    sub = "Simple"
  )

  lm <- lm(length ~ age + I(age^2), data)
  summ <- summary(lm)
  sigma <- sigma(lm)
  r_squared <- summ$r.squared
  adj_r_squared <- summ$adj.r.squared

  coefficients <- lm$coefficients
  print(coefficients)
  intercept <- coefficients["(Intercept)"]
  agecoeff <- coefficients["age"]
  agesqcoeff <- coefficients["I(age^2)"]
  eqn <- paste(sprintf("%0.4f", intercept),
               sprintf("%+0.4f", agecoeff),
               "age",
               sprintf("%+0.4f", agesqcoeff),
               "age^2")
  labels <- c(paste("Sigma: ", sprintf("%+0.4f", sigma)),
  paste("R Squared: ", sprintf("%+0.4f", r_squared)),
  paste("Adj R Squared: ", sprintf("%+0.4f", adj_r_squared)))
  # scatter plot
   plot(data,
    type = "p",
    pch = 16,
    main = "Fitted Line Plot",
    sub = eqn
  )
  curve(intercept + agecoeff * x + agesqcoeff * (x^2),
        add = TRUE,
  col = "red")
  legend("bottomright", legend = labels)

  newdata <-
    data.frame(
               age = 5)
  prediction <- predict(lm, newdata, se.fit = TRUE,
  interval = "prediction")
  print("Prediction interval for age = 5")
  print(prediction$fit)
  prediction <- predict(lm, newdata, se.fit = TRUE,
  interval = "confidence")
  print("Confidence interval for age = 5")
  print(prediction$fit)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

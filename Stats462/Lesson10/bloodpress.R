#!/usr/bin/env Rscript
bloodpress.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/bloodpress.txt"
  )
}

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
source(lib_path())
library(scatterplot3d)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(PerformanceAnalytics))

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(bloodpress.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  scatterplot_matrix(data, "Scatterplots for bloodpress")
  analysis_uncorrelated(data)
  analysis_correlated(data)
  analysis_vif(data)
  return(0)
}

analysis_uncorrelated <- function(data) {
  model <- lm(BP ~ Stress, data)
  complete <- complete_anova(model)
  print(complete)

  model <- lm(BP ~ BSA, data)
  complete <- complete_anova(model)
  print(complete)

  model <- lm(BP ~ Stress + BSA, data)
  complete <- complete_anova(model)
  print(complete)

  model <- lm(BP ~ BSA + Stress, data)
  complete <- complete_anova(model)
  print(complete)

  with(
    data,
    scatterplot3d(BP, BSA, Stress,
      pch = 15,
      type = "h",
      color = "steelblue"
    )
  )
}

analysis_vif <- function(data) {
  model <- lm(BP ~ BSA + Stress + Age + Weight + Pulse + Dur, data)
  coeffs <- model_coeffs(model)
  vifs <- coeffs[
    ,
    grepl(
      "[.]vif",
      names(coeffs)
    )
  ]
  vifs <- vifs[, (vifs[1, ]) > 4]
  print(vifs)

  wt_model <- lm(Weight ~ BSA + Age + Dur + Pulse + Stress, data)
  stats <- model_fit_stats(wt_model)
  r.squared <- stats$R.squared
  vif.weight <- 1 / (1 - r.squared)
  names(vif.weight) <- "vif.Weight"
  print(vif.weight)

  datanew <- data %>%
    select(Weight, BSA, Age, Dur, Pulse, Stress)
  chart.Correlation(datanew, histogram = TRUE, pch = 19, col = "blue")

  model <- lm(BP ~ Weight + Age + Dur + Stress, data)
  print(anova(model))
  print(model_fit_stats(model))
  print(model_coeffs(model))
}

analysis_correlated <- function(data) {
  model <- lm(BP ~ BSA, data)
  complete <- complete_anova(model)
  print(complete)
  new <- data.frame(BSA = 2)
  pred.pi <- predict(model, new, interval = "prediction")
  pred.ci <- predict(model, new, interval = "confidence")
  print("Prediction interval")
  print(pred.pi)
  print("Confidence interval")
  print(pred.ci)

  model <- lm(BP ~ Weight, data)
  complete <- complete_anova(model)
  print(complete)
  new <- data.frame(Weight = 92)
  pred.pi <- predict(model, new, interval = "prediction")
  pred.ci <- predict(model, new, interval = "confidence")
  print("Prediction interval")
  print(pred.pi)
  print("Confidence interval")
  print(pred.ci)

  model <- lm(BP ~ BSA + Weight, data)
  complete <- complete_anova(model)
  print(complete)
  new <- data.frame(Weight = 92, BSA = 2)
  pred.pi <- predict(model, new, interval = "prediction")
  pred.ci <- predict(model, new, interval = "confidence")
  print("Prediction interval")
  print(pred.pi)
  print("Confidence interval")
  print(pred.ci)

  model <- lm(BP ~ Weight + BSA, data)
  complete <- complete_anova(model)
  print(complete)

  with(data, {
    scatterplot3d(BP, BSA, Weight,
      pch = 15,
      color = "steelblue",
      type = "h"
    )
    par(mar = c(5, 6, 4, 4))
    plot(BSA, Weight,
      main = "Weight versus BSA",
      col = "blue", pch = 15
    )
  })
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

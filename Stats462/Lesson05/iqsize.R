#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
source("libfunc.R")

main <- function(argv) {
  data <- read.table("../Data/iqsize.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(PIQ ~ Brain, data = data)
  anova_df <- anova(reg)
  model_df <- model_fit_stats(reg)
  coeff_df <- model_coeffs(reg)
  df <- cbind(coeff_df, model_df)
  reg <- lm(PIQ ~ Brain + Height, data = data)
  anova_df <- rbind(anova_df, anova(reg))
  model_df <- model_fit_stats(reg)
  coeff_df <- model_coeffs(reg)
  df <- dplyr::bind_rows(df, cbind(coeff_df, model_df))
  reg <- lm(PIQ ~ Brain + Height + Weight, data = data)
  anova_df <- rbind(anova_df, anova(reg))
  model_df <- model_fit_stats(reg)
  coeff_df <- model_coeffs(reg)
  df <- dplyr::bind_rows(df, cbind(coeff_df, model_df))
  df %<>% select(
    "(Intercept)",
    "(Intercept).p",
    "Brain",
    "Brain.p",
    "Height",
    "Height.p",
    "Weight",
    "Weight.p",
    "Sigma",
    "R.squared",
    "Adj.R.squared",
    "Ratio.Adj.R2.to.R2",
    "Pred.R.squared",
    "PRESS",
    "p.value"
  )
  print(df)
  print(anova_df)

  scatterplot_matrix(data, "IQ Scatterplot Matrix")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

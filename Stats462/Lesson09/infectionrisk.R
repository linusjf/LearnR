#!/usr/bin/env Rscript
infectionrisk.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/infection_risk.txt"
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
library(car)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table(infectionrisk.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  scatter_plot(data)
  analyze(data)
  return(0)
}

scatter_plot <- function(data) {
  model <- lm(InfctRsk ~ Stay, data)
  eqn <- model_equation(model, digits = 4)
  plot(data$Stay, data$InfctRsk,
    xlab = "Hospital Stay", ylab = "Infection
       Risk", sub = eqn, col.sub = "red",
    main = "Scatter plot of Infection Risk versus Hospital Stay"
  )
  abline(model, col = "red")
  stats <- model_fit_stats(model)
  labels <- c(
    paste0("S : ", stats$Sigma),
    paste0("R squared: ", stats$R.squared),
    paste0("Adj R squared: ", stats$Adj.R.squared)
  )
  legend("topright", legend = labels)
}

analyze <- function(data) {
  orig_model <- lm(InfctRsk ~ Stay, data)
  print("Original equation")
  print(model_equation(orig_model))
  outliers <- detect_outliers(data, orig_model)
  analyze_for_dffits(data, outliers, orig_model)
  analyze_for_cooks(data, orig_model)
}

analyze_for_cooks <- function(data, model) {
  data %<>%
    mutate(cooksdistance = cooks.distance(model))
  range <- c(min(data$cooksdistance), max(data$cooksdistance))
  index <- seq_len(nrow(data))
  plot(data$cooksdistance, index,
    main = "Dot plot for Infection Risk versus Hospital Stay",
    xlab = "Cook's distance",
    ylab = "Index",
    xlim = range
  )
}

analyze_for_dffits <- function(data, outliers, orig_model) {
  data1 <- data %>%
    filter(!(InfctRsk %in% outliers$InfctRsk &
      Stay %in% outliers$Stay))
  model <- lm(InfctRsk ~ Stay, data1)
  print("Modified equation w/o outliers")
  print(model_equation(model))
  data %<>%
    mutate(dffits = dffits(orig_model,
      infl = lm.influence(orig_model)
    ))
  data1 <- data %>%
    filter(InfctRsk %in% outliers$InfctRsk &
      Stay %in% outliers$Stay)
  print("dffit for outliers")
  print(data1$dffits)
}

detect_outliers <- function(data, model) {
  outliers <- outlierTest(model)
  return(data[names(outliers$rstudent), ])
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

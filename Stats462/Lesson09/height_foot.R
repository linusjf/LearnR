#!/usr/bin/env Rscript
heightfoot.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/height_foot.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
library(car)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table(heightfoot.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  scatter_plot(data)
  analyze(data)
  return(0)
}

scatter_plot <- function(data) {
  plot(data$height, data$foot, xlab = "height", ylab = "foot", main = "Scatter plot of foot versus height")
}

analyze <- function(data) {
  orig_model <- lm(foot ~ height, data)
  print("Original equation")
  print(model_equation(orig_model))
  outliers <- detect_outliers(data, orig_model)
  analyze_for_dffits(data, outliers, orig_model)
  analyze_for_cooks(data, orig_model)
}

analyze_for_cooks <- function(data, model) {
  data %<>%
    mutate(cooksdistance = cooks.distance(model))
  old <- par(mar = c(6, 6, 4, 4))
  dotchart(data$cooksdistance,
    main = "Dot plot for Height versus foot", xlab = "Cook's distance",
    ylab = "Index", pch = 19
  )
  par(old)
}

analyze_for_dffits <- function(data, outliers, orig_model) {
  data1 <- data %>%
    filter(!(height %in% outliers$height & foot %in% outliers$foot))
  model <- lm(foot ~ height, data1)
  print("Modified equation w/o outliers")
  print(model_equation(model))
  data %<>%
    mutate(dffits = dffits(orig_model, infl = lm.influence(orig_model)))
  data1 <- data %>%
    filter(height %in% outliers$height & foot %in% outliers$foot)
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

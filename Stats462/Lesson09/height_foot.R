#!/usr/bin/env Rscript
heightfoot.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/height_foot.txt"
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
suppressPackageStartupMessages(library(qualityTools))
library(car)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table(heightfoot.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  scatter_plot(data)
  analyze(data)
  return(0)
}

scatter_plot <- function(data) {
  plot(data$height, data$foot, xlab = "height", ylab = "foot",
  main = "Scatter plot of foot versus height")
}

analyze <- function(data) {
  model <- lm(foot ~ height, data)
  outliers <- detect_outliers(data, model)
  data1 <- data %>%
    filter(!(height %in% outliers$height &
    foot %in% outliers$foot))
  print(data1)
}

detect_outliers <- function(data, model) {
  outliers <- outlierTest(model)
  return(data[names(outliers$rstudent), ])
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

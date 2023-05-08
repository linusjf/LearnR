#!/usr/bin/env Rscript
influence2.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/influence2.txt")
}

influence3.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/influence3.txt")
}

influence4.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/influence4.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(VGAM))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(MASS))

main <- function(argv) {
  lapply(c(influence2.txt(), influence3.txt(), influence4.txt()), influence)
  return(0)
}

influence <- function(path) {
  data <- read.table(path, header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  data1 <- head(data, -1)
  plot(data1$x, data1$y,
    xlab = "X", ylab = "Y", main = "Scatter plot of y versus x",
    sub = path, xlim = c(min(data$x), max(data$x)), ylim = c(min(data$y), max(data$y))
  )
  data2 <- tail(data, 1)
  points(data2$x, data2$y, col = "red")
  lm <- lm(y ~ x, data)
  hat <- hatvalues(lm)
  data %<>%
    mutate(hat = hat) %>%
    mutate(stdres = stdres(lm))
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, hat,
    xlab = "X", ylab = "Hat values", main = "Scatter plot of hat values versus x",
    xlim = c(min(data$x), max(data$x)), ylim = c(min(hat), max(hat))
  )
  k <- length(lm$coefficients) - 1
  hat_mean <- mean(hat)
  upper_bound <- 3 * hat_mean
  n <- nrow(data)
  upper_bound2 <- 3 * ((k + 1) / n)
  lower_bound2 <- 2 * ((k + 1) / n)
  data2 <- data %>%
    filter(hat > upper_bound | hat > upper_bound2)
  points(data2$x, data2$hat, col = "red")
  data2 <- data %>%
    filter(hat > lower_bound2)
  points(data2$x, data2$hat, col = "orange")
  data2 <- data %>%
    filter(abs(stdres) > 2)
  print(data2)
  points(data2$x, data2$hat, col = "blue", pch = 4)
  abline(v = mean(data$x))
  print("Sum hat values: ")
  print(sum(hat))
  boxplot(hat,
    main = path, xlab = "Hat values", col = "orange", border = "brown",
    horizontal = TRUE, notch = FALSE
  )
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
influence1.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/influence1.txt")
}

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
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(olsrr))

main <- function(argv) {
  data <- c(influence1.txt(), influence2.txt(), influence3.txt(), influence4.txt())
  lapply(data, identify)
  return(0)
}

identify <- function(path) {
  data <- read.table(path, header = TRUE)
  model <- lm(y ~ x, data)
  n <- nrow(data)
  k <- length(model$coefficients) - 1
  df1 <- k + 1
  df2 <- n - k - 1
  data %<>%
    mutate(dffits = dffits(model, infl = lm.influence(model))) %>%
    mutate(cooksdistance = cooks.distance(model, infl = lm.influence(model))) %>%
    mutate(probcook = pf(cooksdistance, df1 = df1, df2 = df2))
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, data$y,
    xlab = "X", ylab = "Y", main = "Scatter plot of y versus x",
    sub = path
  )
  data2 <- tail(data, 1)
  points(data2$x, data2$y, col = "red")
  plot_dffits(data, model, path)
  plot_obj <- ols_plot_dffits(model, print_plot = FALSE)
  plot_obj$plot$labels$title <- paste0(plot_obj$plot$labels$title, "\n", path)
  print(plot_obj)
  plot_obj <- ols_plot_cooksd_bar(model, print_plot = FALSE)
  plot_obj$plot$labels$title <- paste0(plot_obj$plot$labels$title, "\n", path)
  print(plot_obj)
  plot_obj <- ols_plot_cooksd_chart(model, print_plot = FALSE)
  plot_obj$plot$labels$title <- paste0(plot_obj$plot$labels$title, "\n", path)
  print(plot_obj)
  plot_fdistr(data, model, path)
}

plot_fdistr <- function(data, model, path) {
  n <- nrow(data)
  k <- length(model$coefficients) - 1
  df1 <- k + 1
  df2 <- n - k - 1

  x <- seq(0, max(data$x), length = 300)
  plot(x, df(x = x, df1 = df1, df2 = df2), main = paste0(
    "F distribution (", df1,
    ",", df2, ")"
  ), sub = path, type = "l", xlim = c(0, max(data$x)), ylab = "Probability Density Function")
  points(data$x, data$probcook)
  influential <- data %>%
    filter(data$probcook >= 0.5)
  points(influential$x, influential$probcook, col = "red", pch = 4)
}

plot_dffits <- function(data, model, path) {
  n <- nrow(data)
  k <- length(model$coefficients) - 1
  cv <- 2 * sqrt((k + 2) / (n - k - 2))
  miny <- min(data$dffits, -cv)
  maxy <- max(data$dffits, cv)
  if (abs(miny) < maxy) {
    miny <- -maxy
  }
  if (abs(miny) > maxy) {
    maxy <- -miny
  }
  plot(data$x, data$dffits, xlab = "X", ylab = "DFFITS", main = paste0(
    "Standardized dffits\n",
    path
  ), ylim = c(miny, maxy), sub = paste0("Critical value = (+/-)", round(
    cv,
    4
  )))
  # critical horizontal value lines
  abline(h = cv, lty = 2)
  abline(h = -cv, lty = 2)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

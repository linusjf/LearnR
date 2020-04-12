#!/usr/bin/env Rscript
influence1.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence1.txt"
  )
}

influence2.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence2.txt"
  )
}

influence3.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence3.txt"
  )
}

influence4.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence4.txt"
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

main <- function(argv) {
  noinfluence()
  outlieronly()
  leverage()
  all()
  return(0)
}

noinfluence <- function(argv) {
  data <- read.table(influence1.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, data$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x")
}

outlieronly <- function(argv) {
  data <- read.table(influence2.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, data$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x")
  data1 <- head(data, -1)
  plot(data1$x, data1$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x")
  data2 <- tail(data, 1)
  points(data2$x, data2$y, col = "red")
  lm <- lm(y ~ x, data)
  eqn <- model_equation(lm, digits = 4, trim = TRUE)
  abline(lm)
  print(summary(lm))
  lm <- lm(y ~ x, data1)
  eqn2 <- model_equation(lm, digits = 4, trim = TRUE)
  abline(lm, col = "red")
  print(summary(lm))
  labels <- c(eqn, eqn2)
  cols <- c("black", "red")
  legend("bottomright",
    col = cols,
    legend = labels, lty = 1:2,
    text.col = cols
  )
}

leverage <- function(argv) {
  data <- read.table(influence3.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, data$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x")
  data1 <- head(data, -1)
  plot(data1$x, data1$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x",
  xlim = c(min(data$x), max(data$x)),
  ylim = c(min(data$y), max(data$y))
  )
  data2 <- tail(data, 1)
  points(data2$x, data2$y, col = "red")
  lm <- lm(y ~ x, data)
  eqn <- model_equation(lm, digits = 4, trim = TRUE)
  abline(lm)
  print(summary(lm))
  lm <- lm(y ~ x, data1)
  eqn2 <- model_equation(lm, digits = 4, trim = TRUE)
  abline(lm, col = "red")
  print(summary(lm))
  labels <- c(eqn, eqn2)
  cols <- c("black", "red")
  legend("bottomright",
    col = cols,
    legend = labels, lty = 1:2,
    text.col = cols
  )
}

all <- function(argv) {
  data <- read.table(influence4.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, data$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x")
  data1 <- head(data, -1)
  plot(data1$x, data1$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x",
  xlim = c(min(data$x), max(data$x)),
  ylim = c(min(data$y), max(data$y))
  )
  data2 <- tail(data, 1)
  points(data2$x, data2$y, col = "red")
  lm <- lm(y ~ x, data)
  eqn <- model_equation(lm, digits = 4, trim = TRUE)
  abline(lm)
  print(summary(lm))
  lm <- lm(y ~ x, data1)
  eqn2 <- model_equation(lm, digits = 4, trim = TRUE)
  abline(lm, col = "red")
  print(summary(lm))
  labels <- c(eqn, eqn2)
  cols <- c("black", "red")
  legend("bottomright",
    col = cols,
    legend = labels, lty = 1:2,
    text.col = cols
  )
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

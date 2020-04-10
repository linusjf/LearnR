#!/usr/bin/env Rscript
depression.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/depression.txt"
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
library(plot3D)
suppressPackageStartupMessages(
  library(dplyr)
)
library(magrittr)

main <- function(argv) {
  data <- read.table(depression.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  print(head(data))

  scatter(data)

  lm <- analyze_interactions(data)

  plot_fitted_lines(data, lm)

  return(0)
}

plot_fitted_lines <- function(data, lm) {
  scatter(data)
  a <- subset(data, data$TRT == "A")
  b <- subset(data, data$TRT == "B")
  c <- subset(data, data$TRT == "C")

  lm <- lm(y ~ age + x2 + x3 + age * x2 + age * x3, a)
  eqn <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(
    abline(lm, col = "black", lty = 2)
  )
  slope <- lm$coefficients["age"]
  angle <- abs(rad2deg(atan(slope)))
  text(mean(a$age), mean(a$y), eqn, pos = 3, srt = angle)

  lm <- lm(y ~ age + x2 + x3 + age * x2 + age * x3, b)
  eqn <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(
    abline(lm, col = "blue", lty = 3)
  )
  slope <- lm$coefficients["age"]
  angle <- abs(rad2deg(atan(slope)))
  text(mean(b$age), mean(b$y), eqn, pos = 3, srt = angle, col = "blue")

  lm <- lm(y ~ age + x2 + x3 + age * x2 + age * x3, c)
  eqn <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(
    abline(lm, col = "red", lty = 4)
  )
  slope <- lm$coefficients["age"]
  angle <- abs(rad2deg(atan(slope)))
  text(mean(c$age), mean(c$y), eqn, pos = 1, srt = angle, col = "red")
}

analyze_interactions <- function(data) {
  lm <- lm(y ~ age + x2 + x3 + age * x2 + age * x3, data)
  summ <- summary(lm)
  anova <- anova(lm)
  print(summ)
  print(anova)
  return(lm)
}

scatter <- function(data) {
  a <- subset(data, data$TRT == "A")
  b <- subset(data, data$TRT == "B")
  c <- subset(data, data$TRT == "C")
  plot(a$age, a$y,
    main = "y against age scatterplot",
    xlab = "Age", ylab = "Y",
    pch = 19, frame = FALSE,
    ylim = c(min(data$y), max(data$y)),
    xlim = c(min(data$age), max(data$age))
  )
  points(b$age, b$y,
    col = "blue",
    pch = 19
  )
  points(c$age, c$y,
    col = "red",
    pch = 19
  )
  box(which = "plot", lty = "solid")
  labels <- c("A", "B", "C")
  legend("bottomright",
    col = c("black", "blue", "red"),
    title = "TRT",
    legend = labels, lty = 1:3,
    text.col = c("black", "blue", "red")
  )
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

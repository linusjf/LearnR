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
library(e1071)

main <- function(argv) {
  data <- read.table(depression.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  print(head(data))

  scatter(data)

  lm <- analyze_interactions(data)

  plot_fitted_lines(data)

  evaluate_model(lm)

  are_treatments_meaningful(lm, data)
  
  are_interactions_meaningful(lm, data)

  return(0)
}

are_treatments_meaningful <- function(full_lm, data) {
  lm <- lm(y ~ age, data)
  anova <- anova(lm, full_lm)
  print(anova)
}

are_interactions_meaningful <- function(full_lm, data) {
  lm <- lm(y ~ age + x2 + x3, data)
  anova <- anova(lm, full_lm)
  print(anova)
}

evaluate_model <- function(lm) {
  plot(lm, which = 1,
  caption = "Standardised Residuals versus Fitted",
    main = "Residuals plot with interaction terms"
  )

  residuals <- resid(lm)
  probplot(residuals,
    probs = c(0.10, 0.25, 0.5, 0.75, 0.9, 0.99, 0.999),
    xlab = "Residuals",
    ylab = "Probabilities (Percent)"
  )

  ad <- nortest::ad.test(residuals)
  labels <- c(
    paste0("Mean: ", round(mean(residuals), 4)),
    paste0("Stdev: ", round(sd(residuals), 2)),
    paste0("Count: ", round(length(residuals), 2)),
    paste0("AD: ", round(ad$statistic, 4)),
    paste0("p-value: ", round(ad$p.value, 4))
  )
  legend("bottomright", legend = labels)
}

plot_fitted_lines <- function(data) {
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
    title = "Treatment",
    legend = labels, lty = 1:3,
    text.col = c("black", "blue", "red")
  )
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

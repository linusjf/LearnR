#!/usr/bin/env Rscript
realestate.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/realestate.txt"
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
library(e1071)

main <- function(argv) {
  data <- read.table(realestate.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  simple <- fit_simple(data)

  interactions <- fit_interactions(data)

  plot_fitted_lines(data)

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
  plot(lm,
    which = 1,
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
  NoAC <- subset(data, data$Air == 0)
  AC <- subset(data, data$Air == 1)

  lm <- lm(SalePrice ~ SqFeet + Air + Air * SqFeet, NoAC)
  eqnNoAC <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(
    abline(lm, col = "red", lty = 2)
  )

  lm <- lm(SalePrice ~ SqFeet + Air + Air * SqFeet, AC)
  eqnAC <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(
    abline(lm, col = "blue", lty = 3)
  )

  labels <- c(
    paste0("No A/C - ", eqnNoAC),
    paste0("A/C - ", eqnAC)
  )
  legend("topleft",
    col = c("red", "blue"),
    legend = labels, lty = 1:3,
    text.col = c("red", "blue")
  )
}

fit_interactions <- function(data) {
  lm <- lm(SalePrice ~ SqFeet + Air + SqFeet * Air, data)
  summ <- summary(lm)
  anova <- anova(lm)
  print(summ)
  print(anova)
  return(lm)
}

fit_simple <- function(data) {
  lm <- lm(SalePrice ~ SqFeet + Air, data)
  summ <- summary(lm)
  anova <- anova(lm)
  print(summ)
  print(anova)
  return(lm)
}

scatter <- function(data) {
  NoAC <- subset(data, data$Air == 0)
  AC <- subset(data, data$Air == 1)
  plot(NoAC$SqFeet, NoAC$SalePrice,
    main = "Sale Price against area scatterplot",
    xlab = "Square Feet (Area)", ylab = "Sale Price",
    pch = 19, frame = FALSE, col = "red",
    ylim = c(min(data$SalePrice), max(data$SalePrice)),
    xlim = c(min(data$SqFeet), max(data$SqFeet))
  )
  points(AC$SqFeet, AC$SalePrice,
    col = "blue",
    pch = 19
  )
  box(which = "plot", lty = "solid")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

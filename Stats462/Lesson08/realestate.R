#!/usr/bin/env Rscript
realestate.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/realestate.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(e1071)



main <- function(argv) {
  data <- read.table(realestate.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  data <- data[order(data$SqFeet, data$Air), ]

  simple <- fit_simple(data)

  interactions <- fit_interactions(data)

  plot_fitted_lines(data)

  evaluate_model(interactions, data)

  data %<>%
    mutate(lnSalePrice = log(SalePrice)) %>%
    mutate(lnSqFeet = log(SqFeet))

  log <- fit_log(data)

  plot_fitted_lines_log(data)

  evaluate_model(log, data)

  return(0)
}

evaluate_model <- function(lm, data) {
  plot(lm, which = c(1, 3), caption = list("Standardised Residuals versus Fitted", 
    "Scale-Location"), )

  residuals <- resid(lm)
  probplot(residuals, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 0.999), xlab = "Residuals", 
    ylab = "Probabilities (Percent)")
}

plot_fitted_lines_log <- function(data) {
  scatter_log(data)
  NoAC <- subset(data, data$Air == 0)
  AC <- subset(data, data$Air == 1)

  lm <- lm(lnSalePrice ~ lnSqFeet + Air + Air * lnSqFeet, NoAC)
  eqnNoAC <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(abline(lm, col = "red", lty = 2))

  lm <- lm(lnSalePrice ~ lnSqFeet + Air + Air * lnSqFeet, AC)
  eqnAC <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(abline(lm, col = "blue", lty = 3))

  labels <- c(paste0("No A/C - ", eqnNoAC), paste0("A/C - ", eqnAC))
  legend("topleft", cex = 0.8, col = c("red", "blue"), legend = labels, lty = 1:3, 
    text.col = c("red", "blue"))
}

plot_fitted_lines <- function(data) {
  scatter(data)
  NoAC <- subset(data, data$Air == 0)
  AC <- subset(data, data$Air == 1)

  lm <- lm(SalePrice ~ SqFeet + Air + Air * SqFeet, NoAC)
  eqnNoAC <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(abline(lm, col = "red", lty = 2))

  lm <- lm(SalePrice ~ SqFeet + Air + Air * SqFeet, AC)
  eqnAC <- model_equation(lm, digits = 4, trim = TRUE)
  suppressWarnings(abline(lm, col = "blue", lty = 3))

  labels <- c(paste0("No A/C - ", eqnNoAC), paste0("A/C - ", eqnAC))
  legend("topleft", cex = 0.8, col = c("red", "blue"), legend = labels, lty = 1:3, 
    text.col = c("red", "blue"))
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

fit_log <- function(data) {
  lm <- lm(lnSalePrice ~ lnSqFeet + Air + lnSqFeet * Air, data)
  summ <- summary(lm)
  anova <- anova(lm)
  print(summ)
  print(anova)
  return(lm)
}

scatter_log <- function(data) {
  NoAC <- subset(data, data$Air == 0)
  AC <- subset(data, data$Air == 1)
  plot(NoAC$lnSqFeet, NoAC$lnSalePrice, main = "log(Sale Price) against log(area) scatterplot", 
    xlab = "log[Square Feet (Area)]", ylab = "log(Sale Price)", pch = 19, frame = FALSE, 
    col = "red", ylim = c(min(data$lnSalePrice), max(data$lnSalePrice)), xlim = c(min(data$lnSqFeet), 
      max(data$lnSqFeet)))
  points(AC$lnSqFeet, AC$lnSalePrice, col = "blue", pch = 19)
  box(which = "plot", lty = "solid")
}

scatter <- function(data) {
  NoAC <- subset(data, data$Air == 0)
  AC <- subset(data, data$Air == 1)
  plot(NoAC$SqFeet, NoAC$SalePrice, main = "Sale Price against area scatterplot", 
    xlab = "Square Feet (Area)", ylab = "Sale Price", pch = 19, frame = FALSE, 
    col = "red", ylim = c(min(data$SalePrice), max(data$SalePrice)), xlim = c(min(data$SqFeet), 
      max(data$SqFeet)))
  points(AC$SqFeet, AC$SalePrice, col = "blue", pch = 19)
  box(which = "plot", lty = "solid")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

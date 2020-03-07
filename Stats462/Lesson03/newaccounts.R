#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/newaccounts.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  process_new(data)

  process_new1(data)

  return(0)
}

process_new <- function(data) {

  reg <- lm(New ~ Size, data = data)
  outer <- lm(New ~ factor(Size), data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  print("")
  print("†*****************Lack of Fit Anslysis*************†")
  print("")
  anova_single <- anova(reg, outer)
  print(anova_single)
  anova_comb <- anova(lm(New ~ Size + factor(Size), data = data))
  print(anova_comb)

  # nolint start
  predicted <- predict(reg)
  actuals <- data$New
  SST <- sum((data$New - mean(data$New))^2)
  print(paste("Total Sum of Squares: ", SST))
  SSR <- sum((predicted - mean(data$New))^2)
  print(paste("Squared Sum of Residuals: ", SSR))
  SSE <- sum(resid(reg)^2)
  print(paste("Squared Sum of Errors: ", SSE))
  SSE <- anova_single[1, 2]
  print(paste("Squared Sum of Errors: ", SSE))
  SSPE <- anova_single[2, 2]
  print(paste("Pure Error Sum of Squares: ", SSPE))
  SSLF <- anova_single[2, 4]
  print(paste("Lack of Fit Sum of Squares: ", SSLF))
  print("SSLF / SSE :")
  print(SSLF / SSE)
  # nolint end

  plot_accounts(data, reg)
}

process_new1 <- function(data) {
  reg <- lm(New2 ~ Size, data = data)
  outer <- lm(New2 ~ factor(Size), data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  print("")
  print("†*****************Lack of Fit Anslysis*************†")
  print("")
  anova_single <- anova(reg, outer)
  print(anova_single)
  anova_comb <- anova(lm(New2 ~ Size + factor(Size), data = data))
  print(anova_comb)

  # nolint start
  predicted <- predict(reg)
  actuals <- data$New2
  SST <- sum((data$New2 - mean(data$New2))^2)
  print(paste("Total Sum of Squares: ", SST))
  SSR <- sum((predicted - mean(data$New2))^2)
  print(paste("Squared Sum of Residuals: ", SSR))
  SSE <- sum(resid(reg)^2)
  print(paste("Squared Sum of Errors: ", SSE))
  SSE <- anova_single[1, 2]
  print(paste("Squared Sum of Errors: ", SSE))
  SSPE <- anova_single[2, 2]
  print(paste("Pure Error Sum of Squares: ", SSPE))
  SSLF <- anova_single[2, 4]
  print(paste("Lack of Fit Sum of Squares: ", SSLF))
  print("SSLF / SSE :")
  print(SSLF / SSE)
  # nolint end

  plot_accounts_new2(data, reg)
}

plot_accounts_new2 <- function(data, reg) {
  par(mar = c(4, 4, 7, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Size of minimum deposit versus number of new accounts\n",
    coefs["(Intercept)"],
    "+",
    coefs["Size"], "Size"
  )
  plot(data$Size, data$New2,
    main = main_label,
    xlab = "Size of minimum deposit", ylab = "New Accounts",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  box(which = "plot", lty = "solid")
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("bottomright", legends)
  abline(h = mean(data$New), col = "black", lty = "dashed")
}

plot_accounts <- function(data, reg) {
  par(mar = c(4, 4, 7, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Size of minimum deposit versus number of new accounts\n",
    coefs["(Intercept)"],
    "+",
    coefs["Size"], "Size"
  )
  plot(data$Size, data$New,
    main = main_label,
    xlab = "Size of minimum deposit", ylab = "New Accounts",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  box(which = "plot", lty = "solid")
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("bottomright", legends)
  abline(h = mean(data$New), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

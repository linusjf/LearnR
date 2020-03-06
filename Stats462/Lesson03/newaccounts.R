#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/newaccounts.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(New ~ Size, data = data)
  outer <- lm(New ~ factor(Size), data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  print(anova(reg, outer))
  print(anova(lm(New ~ Size + factor(Size), data = data)))
  plot_accounts(data, reg)
  reg <- lm(New2 ~ Size, data = data)
  outer <- lm(New2 ~ factor(Size), data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  print(anova(reg, outer))
  print(anova(lm(New2 ~ Size + factor(Size), data = data)))
  plot_accounts_new2(data, reg)
  return(0)
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
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

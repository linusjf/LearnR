#!/usr/bin/env Rscript
wordrecall.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/wordrecall.txt"
  )
}

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(nortest))
suppressPackageStartupMessages(library(lawstat))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table(wordrecall.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  data %<>%
    mutate(lntime = log(time))
  data %<>%
    mutate(prop_1.25 = (prop^-1.25))

  reg <- lm(prop ~ time, data = data)
  print(reg)
  coefs <- reg$coefficients
  summ <- summary(reg)

  eqn <- paste(
    round(coefs["(Intercept)"], 4),
    round(coefs["time"], 4), "time"
  )
  with(
    data,
    plot(time, prop,
      main = "Scatterplot: prop versus time",
      sub = eqn
    )
  )
  labels <- c(
    paste0("Sigma: ", round(summ$sigma, 4)),
    paste0("R-squared: ", round(summ$r.squared, 4)),
    paste0("Adj R-squared: ", round(summ$adj.r.squared, 4))
  )
  with(data, {
    abline(reg, col = "red")
    lines(lowess(time, prop),
      col = "blue"
    )
    legend("topright", legend = labels)
  })
  plot(reg,
    which = 1,
    caption = "Residuals versus Fitted"
  )

  residuals <- resid(reg)
  plot(data$time, residuals, xlab = "Time", main = "Residuals versus Time")
  abline(h = mean(residuals))

  probplot(residuals,
    probs = c(0.10, 0.25, 0.5, 0.75, 0.9, 0.99),
    xlab = "Residuals",
    ylab = "Probabilities (Percent)"
  )

  ad <- nortest::ad.test(residuals)
  print(ad)
  labels <- c(
    paste0("Mean: ", round(mean(residuals), 4)),
    paste0("Stdev: ", round(sd(residuals), 2)),
    paste0("Count: ", round(length(residuals), 2)),
    paste0("AD: ", round(ad$statistic, 4)),
    paste0("p-value: ", round(ad$p.value, 4))
  )
  legend("bottomright", legend = labels)

  reg <- lm(prop ~ lntime, data = data)
  print(reg)
  coefs <- reg$coefficients
  summ <- summary(reg)

  eqn <- paste(
    round(coefs["(Intercept)"], 4),
    round(coefs["lntime"], 4), "lntime"
  )
  with(
    data,
    plot(lntime, prop,
      main = "Scatterplot: prop versus lntime",
      sub = eqn
    )
  )
  labels <- c(
    paste0("Sigma: ", round(summ$sigma, 4)),
    paste0("R-squared: ", round(summ$r.squared, 4)),
    paste0("Adj R-squared: ", round(summ$adj.r.squared, 4))
  )
  with(data, {
    abline(reg, col = "red")
    lines(lowess(lntime, prop),
      col = "blue"
    )
    legend("topright", legend = labels)
  })
  plot(reg,
    which = 1,
    caption = "Residuals versus Fitted"
  )

  residuals <- resid(reg)
  plot(data$time, residuals, xlab = "log(time)", main = "Residuals versus log(Time)")
  abline(h = mean(residuals))

  probplot(residuals,
    probs = c(0.10, 0.25, 0.5, 0.75, 0.9, 0.99),
    xlab = "Residuals",
    ylab = "Probabilities (Percent)"
  )

  ad <- nortest::ad.test(residuals)
  print(ad)
  labels <- c(
    paste0("Mean: ", round(mean(residuals), 4)),
    paste0("Stdev: ", round(sd(residuals), 2)),
    paste0("Count: ", round(length(residuals), 2)),
    paste0("AD: ", round(ad$statistic, 4)),
    paste0("p-value: ", round(ad$p.value, 4))
  )
  legend("bottomright", legend = labels)

  reg <- lm(prop_1.25 ~ time, data = data)
  print(reg)
  coefs <- reg$coefficients
  summ <- summary(reg)

  eqn <- paste(
    round(coefs["(Intercept)"], 4),
    sprintf("%+0.4f", round(coefs["time"], 4)), "time"
  )
  with(
    data,
    plot(time, prop_1.25,
      main = "Scatterplot: prop_1.25 versus time",
      sub = eqn
    )
  )
  labels <- c(
    paste0("Sigma: ", round(summ$sigma, 4)),
    paste0("R-squared: ", round(summ$r.squared, 4)),
    paste0("Adj R-squared: ", round(summ$adj.r.squared, 4))
  )
  with(data, {
    abline(reg, col = "red")
    lines(lowess(time, prop_1.25),
      col = "blue"
    )
    legend("topright", legend = labels)
  })
  plot(reg,
    which = 1,
    caption = "Residuals versus Fitted"
  )

  residuals <- resid(reg)
  plot(data$time, residuals, xlab = "Time", main = "Residuals versus Time")
  abline(h = mean(residuals))

  probplot(residuals,
    probs = c(0.10, 0.25, 0.5, 0.75, 0.9, 0.99),
    xlab = "Residuals",
    ylab = "Probabilities (Percent)"
  )

  ad <- nortest::ad.test(residuals)
  print(ad)
  labels <- c(
    paste0("Mean: ", round(mean(residuals), 4)),
    paste0("Stdev: ", round(sd(residuals), 2)),
    paste0("Count: ", round(length(residuals), 2)),
    paste0("AD: ", round(ad$statistic, 4)),
    paste0("p-value: ", round(ad$p.value, 4))
  )
  legend("bottomright", legend = labels)

  reg <- lm(prop ~ lntime, data = data)
  print(anova(reg))
  print(summary(reg))

  newdata <-
    data.frame(
      lntime = log(1000)
    )
  prediction <- predict(reg, newdata,
    se.fit = TRUE,
    interval = "confidence"
  )
  print(prediction)
  prediction <- predict(reg, newdata,
    se.fit = TRUE,
    interval = "prediction"
  )
  print(prediction)

  confint <- confint(reg, "lntime")
  print(log(10) * confint)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

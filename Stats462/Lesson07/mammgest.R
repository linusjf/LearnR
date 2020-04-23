#!/usr/bin/env Rscript
mammgest.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/mammgest.txt"
  )
}

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(nortest))
suppressPackageStartupMessages(library(lawstat))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table(mammgest.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  data %<>%
    mutate(lngestation = log(Gestation))

  reg <- lm(Gestation ~ Birthwgt, data = data)
  print(reg)
  coefs <- reg$coefficients
  summ <- summary(reg)

  eqn <- paste(
    round(coefs["(Intercept)"], 4),
    sprintf("%+0.4f", round(coefs["Birthwgt"], 4)), "Birthwgt"
  )
  with(
    data,
    plot(Birthwgt, Gestation,
      main = "Scatterplot: Birthweight versus Gestation",
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
    lines(lowess(Birthwgt, Gestation),
      col = "blue"
    )
    legend("topright", legend = labels)
  })
  plot(reg,
    which = 1,
    caption = "Residuals versus Fitted"
  )

  residuals <- resid(reg)
  plot(data$Birthwgt,
    residuals,
    xlab = "Birth Weight",
    main = "Residuals versus Birth Weight"
  )
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

  reg <- lm(lngestation ~ Birthwgt, data = data)
  print(reg)
  coefs <- reg$coefficients
  summ <- summary(reg)

  eqn <- paste(
    round(coefs["(Intercept)"], 4),
    sprintf("%+0.4f", round(coefs["Birthwgt"], 4)), "Birthwgt"
  )
  with(
    data,
    plot(Birthwgt, lngestation,
      main = "Scatterplot: Birthweight versus log(Gestation)",
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
    lines(lowess(Birthwgt, lngestation),
      col = "blue"
    )
    legend("topright", legend = labels)
  })
  plot(reg,
    which = 1,
    caption = "Residuals versus Fitted"
  )

  residuals <- resid(reg)
  plot(data$Birthwgt, residuals, xlab = "Birth Weight", main = "Residuals versus
       Birth Weight")
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

  print(anova(reg))
  print(summary(reg))

  newdata <-
    data.frame(
      Birthwgt = 50
    )
  prediction <- predict(reg, newdata,
    se.fit = TRUE,
    interval = "prediction"
  )
  print(exp(prediction$fit))

  # for 10 kg increase in Birth Weight
  confint <- confint(reg, "Birthwgt")
  rownames(confint) <- "Gestation increase"
  print(exp(10 * confint))
  return(0)
}


if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
iqsize.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/iqsize.txt")
}

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(nortest))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(lawstat))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(lmtest))

main <- function(argv) {
  data <- read.table(iqsize.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  data <- data[order(data$Brain, data$Height), ]
  reg <- lm(PIQ ~ Brain + Height, data = data)
  print(reg)
  plot(reg, which = 1, caption = "Residuals versus Fitted")

  residuals <- resid(reg)
  plot(data$Brain, residuals, xlab = "Brain", main = "Residuals versus Brain")
  abline(h = mean(residuals))
  plot(data$Height, residuals, xlab = "Height", main = "Residuals versus Height")
  abline(h = mean(residuals))
  hist(residuals)
  plot(density(residuals))
  # A quantile normal plot - good for checking normality
  qqnorm(residuals)
  qqline(residuals)
  normal_sample <- rnorm(length(residuals), 0, sd(residuals))
  q1 <- quantile(residuals, probs = seq(0, 1, 1 / length(residuals)))
  q2 <- quantile(normal_sample, probs = seq(0, 1, 1 / length(residuals)))
  plot(q1, q2, xlab = "Residual Quantiles", ylab = "Theoretical Quantiles", main = "QQ plot")
  abline(a = 0, b = 1)
  plot(data$Weight, residuals, xlab = "Weight", main = "Residuals versus Weight")
  abline(h = mean(residuals))

  probplot(residuals,
    probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99), xlab = "Residuals",
    ylab = "Probabilities (Percent)"
  )

  ad <- ad.test(residuals)
  print(ad)
  labels <- c(paste0("Mean: ", round(mean(residuals), 4)), paste0("Stdev: ", round(
    sd(residuals),
    2
  )), paste0("Count: ", round(length(residuals), 2)), paste0("AD: ", round(
    ad$statistic,
    4
  )), paste0("p-value: ", round(ad$p.value, 4)))
  legend("bottomright", legend = labels)

  shapiro <- shapiro.test(residuals)
  print(shapiro)

  ks <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
  print(ks)

  firstsample <- residuals[seq_len(length(residuals) / 2)]
  secondsample <- residuals[seq(length(residuals) / 2 + 1, length(residuals))]
  vartest <- var.test(firstsample, secondsample)
  print(vartest)

  data %<>%
    cbind(residuals) %>%
    mutate(Group = ifelse(row_number() > n() / 2, "Highest", "Lowest"))
  levene <- levene.test(residuals, data[["Group"]])
  print(levene)

  bptest <- bptest(reg, data = data)
  print(bptest)

  bartlett <- bartlett.test(residuals, data[["Group"]])
  print(bartlett)

  ## Calculate confidence intervals
  newdata <- data.frame(Brain = c(90, 79), Height = c(70, 62))
  predicted <- predict(reg, newdata, interval = "confidence", se.fit = TRUE)
  print(predicted)
  predicted <- predict(reg, newdata, interval = "prediction", se.fit = TRUE)
  print(predicted)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

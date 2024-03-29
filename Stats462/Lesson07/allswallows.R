#!/usr/bin/env Rscript
allswallows.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/allswallows.txt")
}
library(scatterplot3d)
library(e1071)

main <- function(argv) {
  data <- read.table(allswallows.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  # 3D scatter plot
  adults <- subset(data, data$Type == 1)
  lm <- lm(Vent ~ CO2 + O2, adults)
  with(adults, {
    s3d <- scatterplot3d(CO2, O2, Vent,
      type = "p", color = "blue", angle = 55,
      pch = 16, main = "Scatterplot 3d Vent_1 vs CO2_1 vs O2_1", sub = "Adult swallows"
    )
    s3d$plane3d(lm)
  })
  nestlings <- subset(data, data$Type == 0)
  lm <- lm(Vent ~ CO2 + O2, nestlings)
  with(nestlings, {
    s3d <- scatterplot3d(CO2, O2, Vent,
      type = "p", color = "blue", angle = 55,
      pch = 16, main = "Scatterplot 3d Vent vs CO2 vs O2", sub = "Nestling swallows"
    )
    s3d$plane3d(lm)
  })

  lmfull <- lm(Vent ~ CO2 + O2 + Type + CO2 * Type + O2 * Type + CO2 * O2, data)
  print("Full model adjusted R squared")
  print(summary(lmfull)$adj.r.squared)
  lmreduced <- lm(Vent ~ CO2 + O2 + Type, data)
  print("Reduced model adjusted R squared")
  print(summary(lmreduced)$adj.r.squared)
  print(anova(lmreduced, lmfull))

  print(summary(lmreduced))
  plot(lmfull, which = 1, caption = "Residuals vs Fitted", main = "Residuals plot with interaction terms")

  residuals <- resid(lmfull)
  probplot(residuals,
    probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 0.999), xlab = "Residuals",
    ylab = "Probabilities (Percent)"
  )

  ad <- nortest::ad.test(residuals)
  labels <- c(paste0("Mean: ", round(mean(residuals), 4)), paste0("Stdev: ", round(
    sd(residuals),
    2
  )), paste0("Count: ", round(length(residuals), 2)), paste0("AD: ", round(
    ad$statistic,
    4
  )), paste0("p-value: ", round(ad$p.value, 4)))
  legend("bottomright", legend = labels)

  plot(lmreduced, which = 1, caption = "Residuals vs Fitted", main = "Residuals plot with no interaction terms")

  residuals <- resid(lmreduced)
  probplot(residuals,
    probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 0.999), xlab = "Residuals",
    ylab = "Probabilities (Percent)"
  )

  ad <- nortest::ad.test(residuals)
  print(ad)
  labels <- c(paste0("Mean: ", round(mean(residuals), 4)), paste0("Stdev: ", round(
    sd(residuals),
    2
  )), paste0("Count: ", round(length(residuals), 2)), paste0("AD: ", round(
    ad$statistic,
    4
  )), paste0("p-value: ", round(ad$p.value, 4)))
  legend("bottomright", legend = labels)

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

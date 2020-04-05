#!/usr/bin/env Rscript
library(skimr)
source("../Lib/libfunc.R")

main <- function(argv) {
  data <- read.table("../Data/birthsmokers_02.txt",
    header = TRUE, fill = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  lm <- lm(Wgt ~ Gest + Smoke, data)
  print(summary(lm))

  newdata <- data.frame(Gest = c(38, 38), Smoke = c(0, 1))
  prediction <- predict(lm, newdata, se.fit = TRUE,
  interval = "confidence")
  print(prediction$fit)

  lm <- lm(Wgt_0 ~ Gest_0, data)
  print(summary(lm))

  newdata <- data.frame(Gest_0 = c(38), Smoke = c(0))
  prediction <- predict(lm, newdata, se.fit = TRUE,
  interval = "confidence")
  print(prediction$fit)

  lm <- lm(Wgt_1 ~ Gest_1, data)
  print(summary(lm))

  newdata <- data.frame(Gest_1 = c(38), Smoke = c(1))
  prediction <- predict(lm, newdata, se.fit = TRUE,
  interval = "confidence")
  print(prediction$fit)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

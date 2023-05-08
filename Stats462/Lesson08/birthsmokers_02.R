#!/usr/bin/env Rscript
birthsmokers02.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/birthsmokers_02.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())

main <- function(argv) {
  data <- read.table(birthsmokers02.txt(), header = TRUE, fill = TRUE)
  print(head(data))
  print(skimr::skim(data))

  lm <- lm(Wgt ~ Gest + Smoke, data)
  summ <- summary(lm)
  coefficients <- summ$coefficients
  segest <- coefficients["Gest", "Std. Error"]
  newdata <- data.frame(Gest = c(38, 38), Smoke = c(0, 1))
  prediction <- predict(lm, newdata, se.fit = TRUE, interval = "confidence")
  fit <- prediction$fit
  width <- fit[, 3] - fit[, 2]
  names(width) <- c("NS", "S")

  summary <- data.frame(
    `Model estimated using…` = c("all 32 data points"), `SE(Gest)` = c(segest),
    `Width of CI for μY` = paste(toString(names(width)), toString(width))
  )
  lm <- lm(Wgt_0 ~ Gest_0, data)
  summ <- summary(lm)
  coefficients <- summ$coefficients
  segest <- coefficients["Gest_0", "Std. Error"]

  newdata <- data.frame(Gest_0 = c(38), Smoke = c(0))
  prediction <- predict(lm, newdata, se.fit = TRUE, interval = "confidence")
  fit <- prediction$fit
  width <- fit[, 3] - fit[, 2]
  summ0 <- data.frame(
    `Model estimated using…` = c("16 non-smokers"), `SE(Gest)` = c(segest),
    `Width of CI for μY` = c(paste(width, collapse = " "))
  )


  lm <- lm(Wgt_1 ~ Gest_1, data)
  summ <- summary(lm)
  coefficients <- summ$coefficients
  segest <- coefficients["Gest_1", "Std. Error"]

  newdata <- data.frame(Gest_1 = c(38), Smoke = c(1))
  prediction <- predict(lm, newdata, se.fit = TRUE, interval = "confidence")
  fit <- prediction$fit
  width <- fit[, 3] - fit[, 2]
  summ1 <- data.frame(
    `Model estimated using…` = c("16 smokers"), `SE(Gest)` = c(segest),
    `Width of CI for μY` = c(paste(width, collapse = " "))
  )
  summary <- rbind(summary, summ0)
  summary <- rbind(summary, summ1)
  print(summary)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

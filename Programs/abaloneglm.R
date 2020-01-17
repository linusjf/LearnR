#!/usr/bin/env Rscript
library(stargazer)

main <- function() {
  aba <- read.csv("abalone.data", header = T)
  abamf <- aba[aba$Sex != "I", ]
  # exclude infants from the analysis
  lftn <- function(clmn) {
    glm(abamf$Sex ~ clmn, family = binomial)$coef
  }
  loall <- sapply(abamf[, -1], lftn)
  stargazer::stargazer(loall, type = "text")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

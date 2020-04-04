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
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

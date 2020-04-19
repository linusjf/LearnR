#!/usr/bin/env Rscript
blaisdell.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/blaisdell.txt"
  )
}

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(FitAR))

main <- function(argv) {
  data <- read.table(blaisdell.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  model <- lm(comsales ~ indsales, data)

  print(model_coeffs(model))
  print(model_fit_stats(model))
  print(model_equation(model, digits = 4))

  print(dwtest(model))

  # Ljung - BoxQ test
  data %<>%
    mutate(residuals = resid(model))
  with(data,
  print(Box.test(residuals, type = "Ljung-Box"))
  )

  attach(data)
  x <- LjungBoxTest(residuals,
                    k = 2,
                    StartLag = 1)
  plot(x[, 3],
       main = "Ljung-Box Q Test",
       col = "blue", pch = 15,
       ylab = "P-values",
       xlab = "Lag")
  detach(data)

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

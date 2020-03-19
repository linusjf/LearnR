#!/usr/bin/env Rscript

source("libfunc.R")

main <- function(argv) {
  data <- read.table("../Data/iqsize.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(PIQ ~ Brain + Height + Weight, data = data)
  print(reg)
  params <- reg$coefficients
  summ <- summary(reg)
  names <- names(params)
  params <- c(params, summ$sigma,
  summ$r.squared, summ$adj.r.squared)
  names(params) <- c(names, "sigma", "R-squared", "Adj R-squared")
  print(summary(reg))
  print(params)
  df <- as.data.frame(do.call(cbind, as.list(params)))
  print(df)
  # nolint start
  scatterplot_matrix(data, "IQ Scatterplot Matrix")
  # nolint end
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

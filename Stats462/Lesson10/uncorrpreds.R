#!/usr/bin/env Rscript
uncorrpreds.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/uncorrpreds.txt"
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

analysis <- function(data) {
  x1only <- lm(y ~ x1, data)
  print(model_equation(x1only))
  print(model_fit_stats(x1only))
  print(model_coeffs(x1only))
  print(anova(x1only))

  x2only <- lm(y ~ x2, data)
  print(model_equation(x2only))
  print(model_fit_stats(x2only))
  print(model_coeffs(x2only))
  print(anova(x2only))

  x1x2 <- lm(y ~ x1 + x2, data)
  print(model_equation(x1x2))
  print(model_fit_stats(x1x2))
  print(model_coeffs(x1x2))
  print(anova(x1x2))

  x2x1 <- lm(y ~ x2 + x1, data)
  print(model_equation(x2x1))
  print(model_fit_stats(x2x1))
  print(model_coeffs(x2x1))
  print(anova(x2x1))
}

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(uncorrpreds.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  scatterplot_matrix(data, "Scatterplots for uncorrpreds")
  analysis(data)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

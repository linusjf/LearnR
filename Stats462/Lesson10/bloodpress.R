#!/usr/bin/env Rscript
bloodpress.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/bloodpress.txt"
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

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(bloodpress.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  scatterplot_matrix(data, "Scatterplots for bloodpress")
  analysis(data)
  return(0)
}

analysis <- function(data) {
  model <- lm(BP ~ Stress, data)
  complete <- complete_anova(model)
  print(complete)

  model <- lm(BP ~ BSA, data)
  complete <- complete_anova(model)
  print(complete)

  model <- lm(BP ~ Stress + BSA, data)
  complete <- complete_anova(model)
  print(complete)

  model <- lm(BP ~ BSA + Stress, data)
  complete <- complete_anova(model)
  print(complete)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
exerimmun.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/exerimmun.txt"
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
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(PerformanceAnalytics))

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(exerimmun.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  chart.Correlation(data,
    histogram = TRUE,
    pch = 15
  )

  model <- lm(igg ~ oxygen, data)
  print(anova(model))
  print(model_coeffs(model))
  print(model_fit_stats(model))
  print(model_equation(model, digits = 4))

  data %<>%
    mutate(oxygensq = oxygen ^ 2)
  chart.Correlation(data,
    histogram = TRUE,
    pch = 15
  )

  model <- lm(igg ~ oxygen + oxygensq, data)
  print(anova(model))
  print(model_coeffs(model))
  print(model_fit_stats(model))
  print(model_equation(model, digits = 4))

  data %<>%
    mutate(oxygencent = scale(oxygen)) %>%
    mutate(oxygencentsq = oxygencent ^ 2)
  with(data,
  plot(oxygencent, oxygencentsq,
  pch = 15, col = "blue",
  main = "Scatterplot of oxygencent versus oxygencentsq",
  sub = paste0("Correlation = ",
               round(cor(oxygencent, oxygencentsq), 4)))
  )
  
  model <- lm(igg ~ oxygencent + oxygencentsq, data)
  print(anova(model))
  print(model_coeffs(model))
  print(model_fit_stats(model))
  print(model_equation(model, digits = 4))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

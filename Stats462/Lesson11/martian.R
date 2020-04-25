#!/usr/bin/env Rscript
martian.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/martian.txt"
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
  data <- read.table(martian.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  model_full <- lm(weight ~ height + water, data)
  print(anova(model_full))
  print(model_coeffs(model_full))
  print(model_fit_stats(model_full))
  eqn1 <- model_equation(model_full, digits = 4)
  print(eqn1)

  model <- lm(weight ~ height, data)
  print(anova(model))
  print(model_coeffs(model))
  print(model_fit_stats(model))
  eqn2 <- model_equation(model, digits = 4)
  print(eqn2)

  with(data, {
  plot(height, weight, pch = 15,
  col = "blue", main = eqn1, col.sub = "grey",
  sub = eqn2)
  abline(model, col = "grey")
  }
  )
  data %<>%
    mutate(fitted = fitted(model_full))
  with(subset(data, data$water == 0),
  lines(height, fitted, lty = 6))
  with(subset(data, data$water == 10),
  lines(height, fitted, lty = 5))
  with(subset(data, data$water == 20),
  lines(height, fitted, lty = 4))
  labels <- c("0", "10", "20")
  title <- "Water"
  lty <- c(6, 5, 4)
  legend("bottomright",
  title = title,
  lty = lty,
  legend = labels)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

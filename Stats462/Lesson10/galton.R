#!/usr/bin/env Rscript
galton.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/galton.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())

main <- function(argv) {
  data <- read.table(galton.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  attach(data)
  model.1 <- lm(Progeny ~ Parent, data)
  summary(model.1)
  eqn1 <- model_equation(model.1, digits = 4)
  # Estimate Std. Error t value Pr(>|t|) (Intercept) 0.127029 0.006993 18.164
  # 9.29e-06 *** Parent 0.210000 0.038614 5.438 0.00285 **

  model.2 <- lm(Progeny ~ Parent, weights = 1 / SD^2, data)
  summary(model.2)
  eqn2 <- model_equation(model.2, digits = 4)
  # Estimate Std. Error t value Pr(>|t|) (Intercept) 0.127964 0.006811 18.787
  # 7.87e-06 *** Parent 0.204801 0.038155 5.368 0.00302 **

  plot(x = Parent, y = Progeny, ylim = c(0.158, 0.174), panel.last = c(lines(sort(Parent),
    fitted(model.1)[order(Parent)],
    col = "blue"
  ), lines(sort(Parent), fitted(model.2)[order(Parent)],
    col = "red"
  )), col.main = "blue", col.sub = "red", main = eqn1, sub = eqn2)
  legend("topleft", col = c("blue", "red"), lty = 1, inset = 0.02, legend = c(
    "OLS",
    "WLS"
  ))
  detach(data)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

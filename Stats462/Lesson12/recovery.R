#!/usr/bin/env Rscript
recovery.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/long_term_recovery.txt"
  )
}, list())

libfunc <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}, list())

library(skimr)
source(libfunc)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table(recovery.txt,
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  linear_model <- lm(log(prog) ~ days,
    data = data,
  )
  print(linear_model)
  coefs <- linear_model$coefficients
  theta0 <- exp(coefs[1])
  theta1 <- coefs[2]
  nl_model <- nls(prog ~ I(theta0 * exp(theta1 * days)),
  data = data, start = list(theta0 = theta0,
                            theta1 = theta1),
  trace = TRUE)
  print(summary(nl_model))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

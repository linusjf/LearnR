#!/usr/bin/env Rscript
uscensus.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/us_census.txt"
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
  data <- read.table(uscensus.txt,
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  beta_one <- 350
  y1 <- data$population[1]
  beta_two <- log(beta_one / y1 - 1)
  y2 <- data$population[2]
  beta_three <- log(beta_one / y2 - 1) - beta_two
  nl_model <- nls(population ~
                  I(beta1 /
                    (1 + exp(beta2 + beta3 * (year - 1790) / 10)
                     )),
  data = data, start = list(beta1 = beta_one,
                            beta2 = beta_two,
                            beta3 = beta_three),
  trace = TRUE)
  print(summary(nl_model))
  eqn <- exp_model_equation(nl_model, digits = 4)
  with(data, {
  plot(year, population, xlab = "Year",
  ylab = "Population", main = "Fitted line plot",
  sub = eqn)
    curve(predict(
      nl_model,
      newdata =
        data.frame(year = year)
    ),
    add = TRUE, xname = "year"
    )
})
  with(data, {
  plot(year, resid(nl_model), xlab = "Year",
  ylab = "Residuals", main = "Versus Year",
  sub = "Response is population",
  pch = 15, col = "blue")
  abline(h = 0, col = "red")
})
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

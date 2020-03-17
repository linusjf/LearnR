#!/usr/bin/env Rscript
suppressMessages(library(EpiModel))
library(deSolve)
library(readr)

  # infection rate since Corona Virus is unknown set to 1
  # average recovery time for Corona Virus is 7 days
  # epimodel suggests that it is the inverse of average recovery time
  # world population birth and death rates are picked from Wikipedia
  # death rate for Corona Virus is 3.5% on high side
  # using R0 of 2.5 and infection durayion of 14 days,
  # a rough estimate of contact or activity rate per
  # per person per day can be estimated
  # since no person has immunity to Corona Virus,
  # the probability of infection will be close to 1
  # This can be reduced by self-isolation and
  # sanitary habits. Using 0.8 as value.
  # activity rate * prob of infection = beta
  # you could run stochastic version of the model as well
  # on a machine with enough memory

main <- function(argv) {
  deterministic()
  return(0)
}

deterministic <- function() {
param <- param.dcm(
    inf.prob = 0.8,
    act.rate = 3.5 / 14,
    rec.rate = 1 / 14,
    a.rate = 4.67532468e-5,
    ds.rate = 1.94805195e-5,
    di.rate = 1.94805195e-5 * 1.035,
    dr.rate = 1.94805195e-5
  )

  r0 <- 0.8 * (3.5 / 14) / (1 / 14)
  names(r0) <- "R0"
  print(r0)
  init <- init.dcm(
    s.num = 7700000000,
    i.num = 555, # from John Hopkins on 22/1/20
    r.num = 0
  )

  control <- control.dcm(
    type = "SIR",
    nsteps = 365,
    dt = 1
  )
  mod <- EpiModel::dcm(param, init, control)
  print(mod)

  par(mar = c(3.2, 3, 2, 1), mgp = c(2, 1, 0), mfrow = c(1, 2))
  plot(mod,
    popfrac = FALSE, alpha = 0.5,
    lwd = 4, main = "Compartment Sizes"
  )
  plot(mod,
    y = "si.flow", lwd = 4, col = "firebrick",
    main = "Disease Incidence", legend = "n"
  )

  par(mfrow = c(1, 1))
  EpiModel::comp_plot(mod, at = 90, digits = 1)
  EpiModel::comp_plot(mod, at = 180, digits = 1)
  EpiModel::comp_plot(mod, at = 270, digits = 1)
  EpiModel::comp_plot(mod, at = 360, digits = 1)

  summary(mod, at = 90)
  summary(mod, at = 180)
  summary(mod, at = 270)
  summary(mod, at = 360)

}

# world population 7.7 billion
# india population 1.37 billion
population <- list(
  World = 7700000000,
  India = 1370000000
)
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

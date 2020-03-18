#!/usr/bin/env Rscript
suppressMessages(library(EpiModel))
library(deSolve)
library(readr)

# infection rate since Corona Virus is unknown set to 1
# average recovery time for Corona Virus is 7 days
# epimodel suggests that it is the inverse of average recovery time
# world population birth and death rates are picked from Wikipedia
# death rate for Corona Virus is 3.5% on high side
# using R0 of 3.5 and incubation duration of 2-14 days,
# recovery ranges from 2 days to a month
# a rough estimate of contact or activity rate per
# per person per day can be estimated
# since no person has immunity to Corona Virus,
# the probability of infection will be close to 1. Alternatively, use the
# probability of infection from a common cold if that data is available.
# This can be reduced by self-isolation and
# sanitary habits. Using 0.8 as value.
# activity rate * prob of infection = beta
# you could run stochastic version of the model as well
# on a machine with enough memory
# The activity rate can be estimated by using R0/average
# duration of disease


main <- function(argv) {
  deterministic()
  return(0)
}

deterministic <- function() {
  param <- param.dcm(
    inf.prob = 0.8,
    act.rate = 5 / 14,
    rec.rate = 1 / 14,
    a.rate = 4.67532468e-5,
    ds.rate = 1.94805195e-5,
    di.rate = 1.94805195e-5 * 1.035,
    dr.rate = 1.94805195e-5,
    dt.rate = 0.035
  )

  r0 <- NULL
  with(param, {
    r0 <<- inf.prob *
      act.rate / rec.rate
    names(r0) <<- "R0"
  })
  print(r0)

  # from John Hopkins on 22/1/20
  init <- init.dcm(
    s.num = 7700000000,
    i.num = 555,
    r.num = 28
  )

  control <- control.dcm(
    type = "SIR",
    nsteps = 365,
    dt = 1
  )
  mod <- EpiModel::dcm(param, init, control)
  print(str(mod))

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

  print("Max deaths: ")
  print(max(mod$epi$i.num$run1 * param$dt.rate))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

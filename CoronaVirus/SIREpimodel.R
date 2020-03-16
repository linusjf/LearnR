#!/usr/bin/env Rscript
suppressMessages(library(EpiModel))

  # infection rate since Corona Virus is unknown set to 1
  # act.rate left at 1 since it cannot be easily gauged.
  # literature suggests that it can be as high as 2 contacts per minute
  # in a crowded space. is this also the inverse
  # here time is in days
  # is activity rate as rcovery late the inverse of average nunber of contacts?
  # i don't know
  # average recovery time for Corona Virus is 7 days
  # epimodel suggests that it is the inverse of average recovery time
  # world population birth and death rates are picked from Wikipedia
  # death rate for Corona Virus is 3.5% on high side

main <- function(argv) {
  deterministic()
  return(0)
}

stochastic <- function() {
param <- param.icm(
    inf.prob = 0.5,
    act.rate = 0.5,
    rec.rate = 1 / 7,
    a.rate = 4.67532468e-5,
    ds.rate = 1.94805195e-5,
    di.rate = 1.94805195e-5 * 1.035,
    dr.rate = 1.94805195e-5
  )

  init <- init.icm(
    s.num = 7700000000,
    i.num = 555, # from John Hopkins on 22/1/20
    r.num = 28
  )

  control <- control.icm(
    type = "SIR",
    nsteps = 365,
    dt = 0.5
  )
  mod <- EpiModel::icm(param, init, control)
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
  EpiModel::comp_plot(mod, at = 365, digits = 1)

  summary(mod, at = 90)
  summary(mod, at = 180)
  summary(mod, at = 270)
  summary(mod, at = 365)
}

deterministic <- function() {
param <- param.dcm(
    inf.prob = 0.5,
    act.rate = 0.5,
    rec.rate = 1 / 7,
    a.rate = 4.67532468e-5,
    ds.rate = 1.94805195e-5,
    di.rate = 1.94805195e-5 * 1.035,
    dr.rate = 1.94805195e-5
  )

  init <- init.dcm(
    s.num = 7700000000,
    i.num = 555, # from John Hopkins on 22/1/20
    r.num = 28
  )

  control <- control.dcm(
    type = "SIR",
    nsteps = 365,
    dt = 0.5
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
  EpiModel::comp_plot(mod, at = 365, digits = 1)

  summary(mod, at = 90)
  summary(mod, at = 180)
  summary(mod, at = 270)
  summary(mod, at = 365)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

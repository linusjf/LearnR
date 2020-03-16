#!/usr/bin/env Rscript
suppressMessages(library(EpiModel))
library(deSolve)
library(readr)

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
  # using R0 of 2.5 and infection durayion of 14 days,
  # a rough estimate of contact or activity rate per
  # per person per day can be estimated

sir <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  ds <- -par$beta / par$N * par$I * par$S
  di <- par$beta / par$N * par$I * par$S - par$gamma * par$I
  dr <- par$gamma * par$I
  list(c(ds, di, dr), N = par$N)
}

rss <- function(parameters, infected = NULL, init = NULL, popn = NULL) {
  names(parameters) <- c("beta", "gamma")
  parameters[["N"]] <- popn
  out <-
    deSolve::ode(
      y = init,
      times = seq(length(infected)),
      func = sir,
      parms = parameters
    )
  fit <- out[, 3]
  sum((infected - fit)^2)
}

main <- function(argv) {


  data <- readr::read_csv("world_data.csv")
  infected <- data$confirmed
  init <- c(S = population$World - infected[1], I = infected[1], R = 0)
  
  deterministic()
  return(0)
}

deterministic <- function() {
param <- param.dcm(
    inf.prob = 0.5,
    act.rate = 2.5 / 14,
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

# world population 7.7 billion
# india population 1.37 billion
population <- list(
  World = 7700000000,
  India = 1370000000
)
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

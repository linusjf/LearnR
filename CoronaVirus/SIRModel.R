#!/usr/bin/env Rscript
library(deSolve)
library(readr)

sir <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par,{
    ds <- -beta / N * I * S
    di <- beta / N * I * S - gamma * I
    dr <- gamma * I
    list(c(ds, di, dr), N = N)
  })
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

  last_record <- tail(data, 1)
  recovery_rate <- last_record$recovered / last_record$confirmed
  death_rate <- last_record$deaths / last_record$confirmed
  upper_recovery_rate <- 1 - death_rate
  deaths <- last_record$deaths

  print(paste("World Death rate: ", death_rate))
  print(paste("World Recovery rate: ", recovery_rate))

  init <- c(S = population$World - infected[1], I = infected[1], R = 0)

  plot_data <- list(label = "\nSIR model 2019-nCoV World")
  analyse(init, infected, death_rate, recovery_rate, deaths, population$World, plot_data)
  analyse(init, infected, death_rate, upper_recovery_rate, deaths,population$World, plot_data)

  data <- readr::read_csv("india_data.csv")
  infected <- data$confirmed
  last_record <- tail(data, 1)
  deaths <- last_record$deaths

  init <- c(S = population$India - infected[1], I = infected[1], R = 0)

  plot_data <- list(label = "\nSIR model 2019-nCoV India")
  analyse(init, infected, death_rate, recovery_rate, deaths,population$India, plot_data)
  analyse(init, infected, death_rate, upper_recovery_rate, deaths,population$India, plot_data)
  recovery_rate <- last_record$recovered / last_record$confirmed
  death_rate <- last_record$deaths / last_record$confirmed
  upper_recovery_rate <- 1 - death_rate

  print("Analysing with India specific rates")
  print(paste("India Death rate: ", death_rate))
  print(paste("India Recovery rate: ", recovery_rate))
  analyse(init, infected, death_rate, recovery_rate, deaths,population$India,
          plot_data)
  analyse(init, infected, death_rate, upper_recovery_rate,
          deaths,population$India, plot_data)
  return(0)
}

analyse <- function(init,
                    infected,
                    death_rate,
                    recovery_rate,
                    deaths,
                    popn,
                    plot_data) {

  # optimize with some sensible conditions
  opt <- optim(c(0.5, recovery_rate),
    rss,
    method = "L-BFGS-B",
    lower = c(0, max(0, recovery_rate - 0.01)), upper =
      c(1, min(recovery_rate + 0.01, 1)),
    infected = infected, init = init,
    popn = popn
  )
  print(opt$message)

  opt_par <- setNames(opt$par, c("beta", "gamma"))
  opt_par[["N"]] <- popn 

  # time in days
  t <- 1:365
  fit <- data.frame(
    deSolve::ode(
      y = init, times = t,
      func = sir, parms =
        opt_par
    )
  )
  # colour
  col <- 1:3

  matplot(fit$time, fit[, 2:4],
    type = "l", xlab = "Day", ylab = "Number of
          subjects", lwd = 2, lty = 1,
    col = col
  )
  title(paste(
    "Recovery rate: ",
    recovery_rate, plot_data$label
  ),
  outer = TRUE, line = -2
  )
  legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"),
    lty = 1,
    lwd = 2, col = col, inset = 0.05
  )

  suppressWarnings(matplot(fit$time, fit[, 2:4], type = "l", xlab = "Day", ylab
                           = "Number of
          subjects", lwd = 2, lty = 1, col = col, log = "y"))

  points(seq(length(infected)), infected)
  legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"),
    lty = 1,
    lwd = 2, col = col, inset = 0.05
  )
  title(paste(
    "Recovery rate: ",
    recovery_rate, plot_data$label
  ),
  outer = TRUE, line = -2
  )

  r0 <- setNames(opt_par["beta"] / opt_par["gamma"], "R0")
  print(r0)

  fit[fit$I == max(fit$I), "I", drop = FALSE]
  # height of pandemic

  print(paste0(
    "Maximum deaths: ",
    max(round(max(fit$I) * death_rate), deaths)
  ))
}

# world population 7.7 billion
# india population 1.37 billion
population <- list(World = 7700000000,
India = 1370000000)

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
library(deSolve)
library(readr)

sir <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  # lambda - F force of infection
  lambda <- par$beta / par$N * par$I
  ds <- -lambda * par$S
  di <- lambda * par$S - par$gamma * par$I
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

  last_record <- tail(data, 1)
  death_rate <- last_record$deaths / last_record$confirmed
  deaths <- last_record$deaths

  print(paste("World Death rate: ", death_rate))

  init <- c(S = population$World - infected[1], I = infected[1], R = 0)

  plot_data <- list(label = "\nSIR model 2019-nCoV World")
  analyse(
    init, infected, death_rate, deaths, population$World,
    plot_data
  )

  data <- readr::read_csv("india_data.csv")
  infected <- data$confirmed
  last_record <- tail(data, 1)
  deaths <- last_record$deaths

  init <- c(S = population$India - infected[1], I = infected[1], R = 0)

  plot_data <- list(label = "\nSIR model 2019-nCoV India")
  analyse(
    init, infected, death_rate, deaths, population$India,
    plot_data
  )
  death_rate <- last_record$deaths / last_record$confirmed

  return(0)
}

analyse <- function(init,
                    infected,
                    death_rate,
                    deaths,
                    popn,
                    plot_data) {

  # optimize with some sensible conditions
  opt <- optim(c(1, 1 / 14),
    rss,
    method = "L-BFGS-B",
    lower = c(0, 1 / 14), upper =
      c(1, 1 / 5),
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
    opt_par[["gamma"]], plot_data$label
  ),
  outer = TRUE, line = -2
  )
  legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"),
    lty = 1,
    lwd = 2,
    col = col,
    inset = 0.05
  )

  suppressWarnings(matplot(fit$time, fit[, 2:4],
    type = "l", xlab = "Day", ylab
    = "Number of
          subjects",
    lwd = 2,
    lty = 1,
    col = col,
    log = "y"
  ))

  points(seq(length(infected)), infected)
  legend("bottomright",
         c("Susceptibles",
           "Infecteds",
           "Recovereds"),
    lty = 1,
    lwd = 2, col = col, inset = 0.05
  )
  title(paste(
    "Recovery rate: ",
   opt_par[["gamma"]], plot_data$label
  ),
  outer = TRUE, line = -2
  )

  r0 <- setNames(opt_par["beta"] / opt_par["gamma"], "R0")
  print(r0)
  print(opt_par)

  fit[fit$I == max(fit$I), "I", drop = FALSE]
  # height of pandemic

  print(paste0(
    "Maximum deaths: ",
    max(round(max(fit$I) * death_rate), deaths)
  ))
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

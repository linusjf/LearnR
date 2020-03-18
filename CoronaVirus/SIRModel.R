#!/usr/bin/env Rscript
library(deSolve)
library(readr)

parms <-
  list(
    gamma = 1 / 14,
    gamma_lcl = 1 / 15,
    gamma_ucl = 1 / 13,
    inf = 1,
    inf_ucl = 1,
    inf_lcl = 0.8,
    act = 5 / 14,
    act_ucl = 9 / 14,
    act_lcl = 2 / 14
  )

# world population 7.7 billion
# india population 1.37 billion
population <- list(
  World = 7700000000,
  India = 1370000000
)

sir <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  # lambda - F force of infection
  lambda <- par$inf * par$act / par$N * par$I
  ds <- -lambda * par$S
  di <- lambda * par$S - par$gamma * par$I
  dr <- par$gamma * par$I
  list(c(ds, di, dr), N = par$N)
}

rss <- function(parameters, infected = NULL, init = NULL, popn = NULL) {
  names(parameters) <- c("inf", "act", "gamma")
  parameters[["N"]] <- popn
  out <-
    deSolve::ode(
      y = init,
      times = seq(length(infected)),
      func = sir,
      parms = parameters
    )
  fit <- out[, "I"]
  sum((infected - fit)^2)
}

main <- function(argv) {
  data <- readr::read_csv("world_data.csv")
  infected <- data$confirmed
  dates <- data$date
  recovered <- data$recovered

  last_record <- tail(data, 1)
  death_rate <- last_record$deaths / last_record$confirmed
  deaths <- last_record$deaths

  print(paste("World Death rate: ", death_rate))

  init <- c(S = population$World - infected[1],
              I = infected[1],
              R = recovered[1])

  plot_data <- list(label = "SIR model 2019-nCoV World")
  analyse(
    init, infected, dates, death_rate, deaths, population$World,
    plot_data
  )

  data <- readr::read_csv("india_data.csv")
  infected <- data$confirmed
  dates <- data$date
  recovered <- data$recovered
  last_record <- tail(data, 1)
  deaths <- last_record$deaths

  init <- c(
    S = population$India - infected[1], I = infected[1], R =
      recovered[1])

  plot_data <- list(label = "SIR model 2019-nCoV India")
  analyse(
    init, infected, dates, death_rate, deaths, population$India,
    plot_data
  )

  return(0)
}

analyse <- function(init,
                    infected,
                    dates,
                    death_rate,
                    deaths,
                    popn,
                    plot_data) {
  opt <- NULL
  # optimize with some sensible conditions
  with(parms, {
    opt <<- optim(c(inf, act, gamma),
      rss,
      method = "L-BFGS-B",
      lower = c(inf_lcl, act_lcl, gamma_lcl),
      upper =
        c(inf_ucl, act_ucl, gamma_ucl),
      control = list(maxit = length(infected) * 100),
      infected = infected, init = init[1:3],
      popn = popn
    )
  })
  print(opt$message)

  opt_par <- setNames(opt$par, c("inf", "act", "gamma"))
  opt_par[["N"]] <- popn

  # time in days
  t <- 1:365
  fit <- data.frame(
    deSolve::ode(
      y = init[1:3], times = t,
      func = sir, parms =
        opt_par
    )
  )
  # colour
  col <- 1:3

  start_date <- dates[1]
  matplot(fit$time, fit[, 2:4],
    type = "l", xlab = "Date", ylab = "Number of
          subjects", lwd = 2, lty = 1,
    col = col, xaxt = "n"
  )
  axis(1, at = fit$time[fit$time %% 100 == 0],
       labels = seq(from = start_date,
                    length.out = 365,
                    by = "day")[c(100, 200, 300)])
  title(plot_data$label
  ,
  outer = TRUE, line = -2
  )
  legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"),
    lty = 1,
    lwd = 2,
    col = col,
    inset = 0.05
  )

  suppressWarnings(matplot(fit$time, fit[, 2:4],
    type = "l", xlab = "Date", ylab
    = "Number of
          subjects",
    lwd = 2,
    lty = 1,
    col = col,
    log = "y", xaxt = "n"
  ))
  axis(1, at = fit$time[fit$time %% 100 == 0],
       labels = seq(from = start_date,
                    length.out = 365,
                    by = "day")[c(100, 200, 300)])

  points(seq(length(infected)), infected)
  legend("bottomright",
    c(
      "Susceptibles",
      "Infecteds",
      "Recovereds"
    ),
    lty = 1,
    lwd = 2, col = col, inset = 0.05
  )
  title(plot_data$label
  ,
  outer = TRUE, line = -2
  )

  r0 <- setNames(opt_par["inf"] * opt_par["act"] / opt_par["gamma"],
                 "R0")
  print(r0)
  names(opt_par) <- c(
    "prob of infection (inf)",
    "activity rate (act)",
    "Rate of recovery (gamma)",
    "Total population"
  )
  print(opt_par)

  fit[fit$I == max(fit$I), "I", drop = FALSE]
  # height of pandemic
  print(paste0(
    "Maximum infected: ",
    max(round(fit$I))
  ))
  print(paste0(
    "Maximum deaths: ",
    max(round(max(fit$I) * death_rate), deaths)
  ))
}


if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

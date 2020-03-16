#!/usr/bin/env Rscript
library(deSolve)
library(readr)

# nolint start
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta / N * I * S
    dI <- beta / N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}
# nolint end

# nolint start
RSS <- function(parameters, infected = NULL, init = NULL, gamma = NULL) {
  names(parameters) <- c("beta", "gamma")
  out <-
    deSolve::ode(
      y = init,
      times = seq(length(infected)), func = SIR, parms = parameters
    )
  fit <- out[, 3]
  sum((infected - fit)^2)
}
# nolint end

main <- function(argv) {
  data <- readr::read_csv("world_data.csv")
  infected <- data$confirmed
  dates <- data$date
  last_record <- tail(data, 1)
  recovery_rate <- last_record$recovered / last_record$confirmed
  death_rate <- last_record$deaths / last_record$confirmed
  upper_recovery_rate <- 1 - death_rate


  print(paste("World Death rate: ", death_rate))
  print(paste("World Recovery rate: ", recovery_rate))

  init <- c(S = N - infected[1], I = infected[1], R = 0)

  plot_data <- list(label = "\nSIR model 2019-nCoV World")
  analyse(init, infected, death_rate, recovery_rate, plot_data)
  analyse(init, infected, death_rate, upper_recovery_rate, plot_data)

  data <- readr::read_csv("india_data.csv")
  infected <- data$confirmed
  init <- c(S = N_INDIA - infected[1], I = infected[1], R = 0)

  plot_data <- list(label = "\nSIR model 2019-nCoV India")
  analyse(init, infected, death_rate, recovery_rate, plot_data)
  analyse(init, infected, death_rate, upper_recovery_rate, plot_data)
  last_record <- tail(data, 1)
  recovery_rate <- last_record$recovered / last_record$confirmed
  death_rate <- last_record$deaths / last_record$confirmed
  upper_recovery_rate <- 1 - death_rate

  print("Analysing with India specific rates")
  print(paste("India Death rate: ", death_rate))
  print(paste("India Recovery rate: ", recovery_rate))
  analyse(init, infected, death_rate, recovery_rate, plot_data)
  analyse(init, infected, death_rate, upper_recovery_rate, plot_data)
  return(0)
}

analyse <- function(init, infected, death_rate, recovery_rate, plot_data) {

  # optimize with some sensible conditions
  opt <- optim(c(0.5, recovery_rate),
    RSS,
    method = "L-BFGS-B",
    lower = c(0, max(0, recovery_rate - 0.01)), upper =
      c(1, min(recovery_rate + 0.01, 1)),
    infected = infected, init = init
  )
  print(opt$message)

  opt_par <- setNames(opt$par, c("beta", "gamma"))
  print(opt_par)

  # time in days
  t <- 1:365
  fit <- data.frame(
    deSolve::ode(
      y = init, times = t,
      func = SIR, parms =
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

  suppressWarnings(matplot(fit$time, fit[, 2:4], type = "l", xlab = "Day", ylab = "Number of
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

  # nolint start
  R0 <- setNames(opt_par["beta"] / opt_par["gamma"], "R0")
  # nolint end
  print(R0)

  fit[fit$I == max(fit$I), "I", drop = FALSE]
  # height of pandemic

  print(paste0(
    "Maximum deaths: ",
    round(max(fit$I) * death_rate)
  ))
}

# nolint start
# world population 7.7 billion
N <- 7700000000
# nolint end
# india population 1.37 billion
N_INDIA <- 1370000000
# nolint end
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

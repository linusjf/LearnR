#!/usr/bin/env Rscript

# using the "ode" function
library(deSolve)

parameters_values <- c(
  # infectious contact rate (/person/day)
  beta = 0.004,
  # recovery rate (/day)
  gamma = 0.5
)
initial_values <- c(
  # number of susceptibles at time = 0
  S = 999,
  # number of infectious at time = 0
  I =   1,
  # number of recovered (and immune) at time = 0
  R =   0
)

main <- function(argv) {
  # days
  time_values <- seq(0, 10)
  print(ls())
  print(sir_equations)
  print(parameters_values)
  print(initial_values)
  print(time_values)

  sir_values <- ode(
    y = initial_values,
    times = time_values,
    func = sir_equations,
    parms = parameters_values
  )

  print(sir_values)

  sir_values <- as.data.frame(sir_values)
  print(sir_values)
  plot_it(sir_values)
  r0 <- (999 + 1) * parameters_values["beta"] / parameters_values["gamma"]
  names(r0) <- "R0"
  print(r0)
  return(0)
}

plot_it <- function(data) {
  with(data, eval(parse(
    text =
      "{
    # plotting the time series of susceptibles:
    plot(time, S,
      type = 'l', col = 'blue',
      xlab = 'time (days)', ylab = 'number of people'
    )
    # adding the time series of infectious:
    lines(time, I, col = 'red')
    # adding the time series of recovered:
    lines(time, R, col = 'green')
  }"
  )))

  # adding a legend:
  legend("right", c("susceptibles", "infectious", "recovered"),
    col = c("blue", "red", "green"), lty = 1, bty = "n"
  )
}
# nolint start
sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <- beta * I * S - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}
# nolint end

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

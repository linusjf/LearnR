#!/usr/bin/env Rscript
suppressMessages(library(tseries))
# Goal: Utilise matrix notation We use the problems of portfolio analysis as an
# example.
main <- function(argv) {
  # Prices of 4 firms to play with, at weekly frequency (for calendar 2004) --
  p <- structure(
    c(
      300.403, 294.604, 291.038, 283.805, 270.773, 275.506, 292.271,
      292.837, 284.872, 295.037, 280.939, 259.574, 250.608, 268.84, 266.507, 263.94,
      273.173, 238.609, 230.677, 192.847, 219.078, 201.846, 210.279, 193.281, 186.748,
      197.314, 202.813, 204.08, 226.044, 242.442, 261.274, 269.173, 256.05, 259.75,
      243, 250.3, 263.45, 279.5, 289.55, 291.95, 302.1, 284.4, 283.5, 287.8, 298.3,
      307.6, 307.65, 311.9, 327.7, 318.1, 333.6, 358.9, 385.1, 53.6, 51.95, 47.65,
      44.8, 44.85, 44.3, 47.1, 44.2, 41.8, 41.9, 41, 35.3, 33.35, 35.6, 34.55,
      35.55, 40.05, 35, 34.85, 28.95, 31, 29.25, 29.05, 28.95, 24.95, 26.15, 28.35,
      29.4, 32.55, 37.2, 39.85, 40.8, 38.2, 40.35, 37.55, 39.4, 39.8, 43.25, 44.75,
      47.25, 49.6, 47.6, 46.35, 49.4, 49.5, 50.05, 50.5, 51.85, 56.35, 54.15, 58,
      60.7, 62.7, 293.687, 292.746, 283.222, 286.63, 259.774, 259.257, 270.898,
      250.625, 242.401, 248.1, 244.942, 239.384, 237.926, 224.886, 243.959, 270.998,
      265.557, 257.508, 258.266, 257.574, 251.917, 250.583, 250.783, 246.6, 252.475,
      266.625, 263.85, 249.925, 262.9, 264.975, 273.425, 275.575, 267.2, 282.25,
      284.25, 290.75, 295.625, 296.25, 291.375, 302.225, 318.95, 324.825, 320.55,
      328.75, 344.05, 345.925, 356.5, 368.275, 374.825, 373.525, 378.325, 378.6,
      374.4, 1416.7, 1455.15, 1380.97, 1365.31, 1303.2, 1389.64, 1344.05, 1266.29,
      1265.61, 1312.17, 1259.25, 1297.3, 1327.38, 1250, 1328.03, 1347.46, 1326.79,
      1286.54, 1304.84, 1272.44, 1227.53, 1264.44, 1304.34, 1277.65, 1316.12, 1370.97,
      1423.35, 1382.5, 1477.75, 1455.15, 1553.5, 1526.8, 1479.85, 1546.8, 1565.3,
      1606.6, 1654.05, 1689.7, 1613.95, 1703.25, 1708.05, 1786.75, 1779.75, 1906.35,
      1976.6, 2027.2, 2057.85, 2029.6, 2051.35, 2033.4, 2089.1, 2065.2, 2091.7
    ),
    .Dim = c(53, 4), .Dimnames = list(NULL, c("TISCO", "SAIL", "Wipro", "Infosys"))
  )

  # Shift from prices to returns --
  r <- 100 * diff(log(p))

  # Historical expected returns --
  cat("Historical expected returns:\n")
  print(colMeans(r))

  # Historical correlation matrix --
  cat("Historical correlation:\n")
  print(cor(r))

  # Historical covariance matrix --
  s <- cov(r)
  cat("Historical covariance:\n")
  print(s)

  # Historical portfolio variance for a stated portfolio of 20%,20%,30%,30% --
  w <- c(0.2, 0.2, 0.3, 0.3)
  cat("Weights:\n")
  cat(w, "\n")
  cat("Historical variance for above weights:\n")
  cat(t(w) %*% s %*% w, "\n")

  # The portfolio optimisation function in tseries -- This uses the historical
  # facts from r
  optimised <- tseries::portfolio.optim(r)

  # Weights
  cat("Weights of optimised portfolio:\n")
  cat(optimised$pw, "\n")
  # Expected return using these weights
  cat("Expected return of optimised portfolio:\n")
  cat(optimised$pm, "\n")
  # Standard deviation of optimised port.
  cat("Standard deviation of optimised portfolio:\n")
  cat(optimised$ps, "\n")

  # The portfolio optimisation function in tseries -- This uses the historical
  # facts from r
  cat("For optimised portfolio with min 10% and max 50%\n")
  optimised <- tseries::portfolio.optim(r, reslow = c(0.1, 0.1, 0.1, 0.1), reshigh = c(
    0.5,
    0.5, 0.5, 0.5
  ))
  # Weights
  cat("Weights of optimised portfolio:\n")
  cat(optimised$pw, "\n")
  # Expected return using these weights
  cat("Expected return of optimised portfolio:\n")
  cat(optimised$pm, "\n")
  # Standard deviation of optimised port.
  cat("Standard deviation of optimised portfolio:\n")
  cat(optimised$ps, "\n")

  cat("For optimised portfolio with riskfree rate 2%\n")
  optimised <- tseries::portfolio.optim(r, riskless = TRUE, rf = 0.02)
  # Weights
  cat("Weights of optimised portfolio:\n")
  cat(optimised$pw, "\n")
  cat("Weight of risk-free security:\n")
  cat(1 - sum(optimised$pw), "\n")
  # Expected return using these weights
  cat("Expected return of optimised portfolio:\n")
  cat(optimised$pm, "\n")
  # Standard deviation of optimised port.
  cat("Standard deviation of optimised portfolio:\n")
  cat(optimised$ps, "\n")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

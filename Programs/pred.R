#!/usr/bin/env Rscript

preda <- function(x, k) {
  n <- length(x)
  k2 <- k / 2
  # the vector pred will contain our predicted values
  pred <- vector(length = n - k)
  for (i in 1:(n - k)) {
    if (sum(x[i:(i + (k - 1))]) >= k2) {
      pred[i] <- 1
    } else {
      pred[i] <- 0
    }
  }
  return(mean(abs(pred - x[(k + 1):n])))
}

predb <- function(x, k) {
  n <- length(x)
  k2 <- k / 2
  pred <- vector(length = n - k)
  sm <- sum(x[1:k])
  if (sm >= k2) {
    pred[1] <- 1
  } else {
    pred[1] <- 0
  }
  if (n - k >= 2) {
    for (i in 2:(n - k)) {
      sm <- sm + x[i + k - 1] - x[i - 1]
      if (sm >= k2) {
        pred[i] <- 1
      } else {
        pred[i] <- 0
      }
    }
  }
  return(mean(abs(pred - x[(k + 1):n])))
}

predc <- function(x, k) {
  n <- length(x)
  k2 <- k / 2
  # the vector red will contain our predicted values
  pred <- vector(length = n - k)
  csx <- c(0, cumsum(x))
  for (i in 1:(n - k)) {
    if (csx[i + k] - csx[i] >= k2) {
      pred[i] <- 1
    } else {
      pred[i] <- 0
    }
  }
  return(mean(abs(pred - x[(k + 1):n])))
}

main <- function(argv) {
  rains <- abs(round(rnorm(mean = 0, sd = 0.33, 500)))

  procstart <- proc.time()
  a <- preda(rains, 3)
  print(a)
  procend <- proc.time() - procstart
  print(procend)

  procstart <- proc.time()
  b <- preda(rains, 3)
  print(b)
  procend <- proc.time() - procstart
  print(procend)

  procstart <- proc.time()
  c <- preda(rains, 3)
  print(c)
  procend <- proc.time() - procstart
  print(procend)

  procstart <- proc.time()
  a <- preda(rains, 4)
  print(a)
  procend <- proc.time() - procstart
  print(procend)

  procstart <- proc.time()
  b <- preda(rains, 4)
  print(b)
  procend <- proc.time() - procstart
  print(procend)

  procstart <- proc.time()
  c <- preda(rains, 4)
  print(c)
  procend <- proc.time() - procstart
  print(procend)

  procstart <- proc.time()
  a <- preda(rains, 5)
  print(a)
  procend <- proc.time() - procstart
  print(procend)

  procstart <- proc.time()
  b <- preda(rains, 5)
  print(b)
  procend <- proc.time() - procstart
  print(procend)

  procstart <- proc.time()
  c <- preda(rains, 5)
  print(c)
  procend <- proc.time() - procstart
  print(procend)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

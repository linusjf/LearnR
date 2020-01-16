#!/usr/bin/env Rscript
# Goal: A stock is traded on 2 exchanges.  Price data is missing at random on
# both exchanges owing to non-trading.  We want to make a single price
# time-series utilising information from both exchanges. I.e., missing data for
# exchange 1 will be replaced by information for exchange 2 (if observed).

main <- function(argv) {
  # Let's create some example data for the problem.  Prices on exchange 1
  e1 <- runif(15)
  print(e1)
  # Prices on exchange 2.
  e2 <- e1 + 0.05 * rnorm(15)
  print(e2)
  cbind(e1, e2)
  # Blow away 5 points from each at random.
  e1[sample(1:15, 5)] <- NA
  e2[sample(1:15, 5)] <- NA
  print(e1)
  print(e2)
  print(cbind(e1, e2))

  # Now how do we reconstruct a time-series that
  # tries to utilise both?  Do use the
  # more liquid exchange here.
  combined <- e1
  missing <- is.na(combined)
  # if it's also missing, I don't care.
  combined[missing] <- e2[missing]
  # There you are.
  print(cbind(e1, e2, combined))
  return(0)
}

if (identical(environment(), globalenv()))
  quit(status = main(commandArgs(trailingOnly = TRUE)))

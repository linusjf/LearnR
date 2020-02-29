#!/usr/bin/env Rscript

main <- function(argv) {
  print(head(qnorm(seq(0.01, 0.99, 0.01))))
  print(head(quantile(rnorm(200), probs = seq(0.01, 0.99, 0.01))))
  qqnorm(trees$Height)
  y <- qunif(ppoints(length(randu$x)))
  qqplot(randu$x, y)
  qqplot(qnorm(ppoints(30)), qchisq(ppoints(30), df = 3))
  qqplot(qnorm(ppoints(30)), qcauchy(ppoints(30)))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

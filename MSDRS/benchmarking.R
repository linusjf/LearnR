#!/usr/bin/env Rscript
library(microbenchmark)

main <- function(argv) {
print(microbenchmark(a <- rnorm(1000),
               b <- mean(rnorm(1000))))
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

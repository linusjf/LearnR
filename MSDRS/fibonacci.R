#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(magrittr)
library(microbenchmark)
suppressMessages(library(purrr))
suppressMessages(library(tidyr))

fib <- function(n) {
  stopifnot(n > 0)
  if (n == 1) {
    return(0)
  } else if (n == 2) {
    return(1)
  } else {
    return(fib(n - 1) + fib(n - 2))
  }
}

fib_mem <- function(n) {
  stopifnot(n > 0)
  if (!is.na(fib_tbl[n])) {
    return(fib_tbl[n])
  } else {
    fib_tbl[n - 1] <<- fib_mem(n - 1)
    fib_tbl[n - 2] <<- fib_mem(n - 2)
    return(fib_tbl[n - 1] + fib_tbl[n - 2])
  }
}

main <- function(argv) {
  print(purrr::map_dbl(1:12, fib))
  print(purrr::map_dbl(1:12, fib_mem))

  fib_data <- map(1:20, function(x) {
    log(microbenchmark::microbenchmark(fib(x),
      times = 100
    )$time)
  })
  names(fib_data) <- paste0(letters[1:20], 1:20)
  fib_data <- as.data.frame(fib_data)

  fib_data %<>%
    gather(num, time) %>%
    group_by(num) %>%
    summarise(med_time = median(time))

  memo_data <- map(1:20, function(x) {
    log(microbenchmark::microbenchmark(fib_mem(x))$time)
  })
  names(memo_data) <- paste0(letters[1:20], 1:20)
  memo_data <- as.data.frame(memo_data)

  memo_data %<>%
    gather(num, time) %>%
    group_by(num) %>%
    summarise(med_time = median(time))

  pdf("speed.pdf")
  plot(1:20, fib_data$med_time,
    xlab = "Fibonacci Number", ylab = "Log Median Time
     (log Nanoseconds)",
    pch = 18, bty = "n", xaxt = "n"
  )
  axis(1, at = 1:20)
  points(1:20 + .1, memo_data$med_time, col = "blue", pch = 18)
  legend(1, max(fib_data$med_time), c("Not Memoized", "Memoized"),
    pch = 18,
    col = c("black", "blue"), bty = "n", cex = 1, y.intersp = 1.5
  )
  graphics.off()
  return(0)
}


fib_tbl <- c(0, 1, rep(NA, 23))
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

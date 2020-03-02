#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("houseprice.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  data <- data[["Price"]]
  upper_tail_test(data)
  return(0)
}

upper_tail_test <- function(data) {
  test_value <- t.test(data,
    mu = 255, conf.level = 0.95,
    alternative = "greater"
  )
  print(test_value)
  df <- test_value$parameter
  statistic <- test_value$statistic
  los_stat <- qt(0.05, df, lower = FALSE)
  plot_upper_tail_test(los_stat, statistic, df)
}

plot_upper_tail_test <- function(los_stat, statistic, df) {
  t_values <- seq(-4, 4, .1)
  par(mar = c(10, 4, 6, 2), mex = 0.8)
  plot(
    main = "Upper tail test: Reject Null",
    x = t_values,
    y = dt(t_values, df),
    type = "l",
    lty = "dotted",
    xlab = "t",
    ylab = "f(t)",
    xaxt = "n"
  )
  ticks <- c(0, los_stat, statistic)
  labels <- formatC(ticks, format = "f", digits = 2)
  labels[2] <- paste0(
    labels[2],
    "\ncritical value"
  )
  labels[3] <- paste0(
    labels[3],
    "\ntest statistic"
  )
  axis(side = 1, at = ticks, labels = labels, las = 2)
  x <- seq(los_stat, 4, length = 200)
  y <- dt(x, df = df)
  polygon(c(los_stat, x, 4), c(0, y, 0), col = "gray50")
  x <- seq(statistic, 4, length = 200)
  y <- dt(x, df = df)
  polygon(c(statistic, x, 4), c(0, y, 0), col = "gray25")
  x0 <- (los_stat + statistic) / 2
  y0 <- dt(x0, df = df) - 0.02
  x1 <- x0 + 0.2
  y1 <- y0 + 0.05
  arrows(x1, y1,
    x1 = x0, y1 = y0, angle = 30,
    code = 2
  )
  text(x1, y1, "significance level")
  x0 <- (statistic + 4) / 2 - 0.2
  y0 <- dt(x0, df = df)
  x1 <- x0 + 0.2
  y1 <- y0 + 0.05
  arrows(x1, y1,
    x1 = x0, y1 = y0, angle = 30,
    code = 2
  )
  text(x1, y1, "p-value")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

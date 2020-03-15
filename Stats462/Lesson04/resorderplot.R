#!/usr/bin/env Rscript
library(basicTrendline)

main <- function(argv) {
  plot_order()
  plot_trends()
  return(0)
}

plot_trends <- function() {
  par(par_old)
  par(
    mfrow = c(2, 2),
    mar = c(4, 4, 1, 1),
    mgp = c(3, 1, 0), las = 0
  )
  basicTrendline::trendline(mtcars$disp, mtcars$mpg)
  basicTrendline::trendline(Loblolly$age, Loblolly$height)
  basicTrendline::trendline(mtcars$qsec, mtcars$hp)
  basicTrendline::trendline(mtcars$drat, mtcars$hp)
}

plot_order <- function() {
  tmp <- lm(mpg ~ disp, data = mtcars)
  plot(tmp$residuals,
    pch = 20,
    xlab = "Order", ylab = "Residuals",
    main = "Good: No Trend", cex.main = 0.95,
    xaxt = "n", yaxt = "n"
  )
  lines(tmp$residuals)

  tmp <- lm(height ~ age, data = Loblolly)
  plot(tmp$residuals,
    pch = 20,
    xlab = "Order", ylab = "Residuals",
    main = "Questionable: General Trend", cex.main = 0.95,
    xaxt = "n", yaxt = "n"
  )
  lines(tmp$residuals)

  tmp <- lm(hp ~ qsec, data = mtcars)
  plot(tmp$residuals,
    pch = 20,
    xlab = "Order", ylab = "Residuals",
    main = "Questionable: Interesting Patterns", cex.main = 0.95,
    xaxt = "n", yaxt = "n"
  )
  lines(tmp$residuals)

  tmp <- lm(hp ~ drat, data = mtcars[order(mtcars$cyl), ])
  plot(tmp$residuals,
    pch = 20,
    xlab = "Order", ylab = "Residuals",
    main = "Bad: Obvious Trend", cex.main = 0.95,
    xaxt = "n", yaxt = "n"
  )
  lines(tmp$residuals)
}

par_old <- par(
  mfrow = c(2, 2), mar = c(2, 2, 1, 1),
  mgp = c(1, 1, 0)
)
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

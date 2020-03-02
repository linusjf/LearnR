#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("houseprice.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  data <- data[["Price"]]
  hist(data,
    main = "Histogram for House Prices",
    xlab = "Price (Sale price in 000s)",
    xlim = c(150, 400),
    las = 1,
    breaks = c(150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400),
    prob = TRUE
  )
  lines(density(data),
    lwd = 2
  )
  x <- seq(-4, 4, length = 100)
  hx <- dnorm(x)

  plot(x, hx,
    type = "l", lty = 2, xlab = "x value",
    ylab = "Density", main = "Normal Distribution"
  )
  x <- seq(0, 4, length = 50)
  y <- dnorm(x)
  polygon(c(0, x, 4), c(0, y, 0), col = "gray")

  x <- seq(-4, 4, length = 200)
  y <- dnorm(x)
  plot(x, y, lty = 2, type = "l", lwd = 2)
  x <- seq(-3, 3, length = 200)
  y <- dnorm(x)
  polygon(c(-3, x, 3), c(0, y, 0), col = "gray")
  x <- seq(-2, 2, length = 200)
  y <- dnorm(x)
  polygon(c(-2, x, 2), c(0, y, 0), col = "gray25")
  x <- seq(-1, 1, length = 200)
  y <- dnorm(x)
  polygon(c(-1, x, 1), c(0, y, 0), col = "gray50")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

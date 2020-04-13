#!/usr/bin/env Rscript
influence2.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence2.txt"
  )
}

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(MASS))

main <- function(argv) {
  studentize(influence2.txt())
  return(0)
}

studentize <- function(path) {
  data <- read.table(path,
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, data$y,
    xlab = "X", ylab = "Y",
    main = "Scatter plot of y versus x",
    sub = path,
    xlim = c(min(data$x), max(data$x)),
    ylim = c(min(data$y), max(data$y))
  )
  lm <- lm(y ~ x, data)
  data %<>%
    mutate(studres = studres(lm))
  print(head(data))
  print(skimr::skim(data))
  data2 <- data %>%
    filter(abs(studres) > 2)
  print(data2)
  points(data2$x, data2$y, col = "red", pch = 4)
  abline(v = mean(data$x))
  boxplot(data$studres,
    main = path,
    xlab = "Studentized residuals",
    col = "pink",
    border = "blue",
    horizontal = TRUE,
    notch = FALSE
  )

  # Display the Student's t distributions with various
  # degrees of freedom and compare to the normal distribution

  x <- seq(-7, 7, length = 300)

  n <- nrow(data)
  k <- 1

  degf <- n - k - 2
  colors <- "darkgreen"
  labels <- paste0("df=", degf)

  plot(x, dt(x, degf),
    type = "l", lty = 2,
    xlab = "x value",
    ylab = "Density",
    main = "t Distribution plot",
    lwd = 2, col = colors
  )

  legend("topright",
    inset = .05,
    title = "t Distribution",
    labels, lwd = 2, lty = 1,
    col = colors
  )
  points(data2$studres, dt(data2$studres, degf), col = "red", pch = 4)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

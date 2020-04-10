#!/usr/bin/env Rscript
depression.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/depression.txt"
  )
}

library(skimr)
library(scatterplot3d)
library(plot3D)
suppressPackageStartupMessages(
  library(dplyr)
)
library(magrittr)

main <- function(argv) {
  data <- read.table(depression.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  print(head(data))

  scatter(data)

  analyze_interactions(data)

  return(0)
}

analyze_interactions <- function(data) {
  lm <- lm(y ~ age + x2 + x3 + age * x2 + age * x3, data)
  summ <- summary(lm)
  anova <- anova(lm)
  print(summ)
  print(anova)
  return(lm)
}

scatter <- function(data) {
  a <- subset(data, data$TRT == "A")
  b <- subset(data, data$TRT == "B")
  c <- subset(data, data$TRT == "C")
  plot(a$age, a$y,
    main = "y against age scatterplot",
    xlab = "Age", ylab = "Y",
    pch = 19, frame = FALSE,
    ylim = c(min(data$y), max(data$y)),
    xlim = c(min(data$age), max(data$age))
  )
  points(b$age, b$y,
    col = "blue",
    pch = 19
  )
  points(c$age, c$y,
    col = "green",
    pch = 19
  )
  box(which = "plot", lty = "solid")
  labels <- c("A", "B", "C")
  legend("bottomright",
  col = c("black", "blue", "green"),
  title = "TRT",
  legend = labels, lty = 1:3,
  text.col = c("black", "blue", "green"))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

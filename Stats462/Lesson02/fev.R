#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/fev_dat.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(FEV ~ age, data = data)
  print(reg)
  print(summary(reg))
  plot_fev(data, reg)
  return(0)
}

plot_fev <- function(data, reg) {
  par(mar = c(4, 7, 4, 1))
  plot(data$age, data$FEV,
  main = "Age versus Forced Exhalation Volume",
    xlab = "Age",
  ylab = "Forced Exhalation Volume (FEV)",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  box(which = "plot", lty = "solid")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

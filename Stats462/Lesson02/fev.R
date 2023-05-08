#!/usr/bin/env Rscript
fev_dat <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/fev_dat.txt")
}
library(skimr)
library(lawstat)

main <- function(argv) {
  data <- read.table(fev_dat(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(FEV ~ age, data = data)
  print(reg)
  print(summary(reg))
  print(levene.test(data[["FEV"]], data[["age"]], location = "median", correction.method = "zero.correction"))
  plot_fev(data, reg)
  return(0)
}

plot_fev <- function(data, reg) {
  par(mar = c(4, 7, 4, 1))
  plot(data$age, data$FEV,
    main = "Age versus Forced Exhalation Volume", xlab = "Age",
    ylab = "Forced Exhalation Volume (FEV)", pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  box(which = "plot", lty = "solid")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

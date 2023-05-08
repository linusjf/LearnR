#!/usr/bin/env Rscript
signdist() <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/signdist.txt")
}
library(skimr)

main <- function(argv) {
  data <- read.table(signdist(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Distance ~ Age, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg, level = 0.99))
  plot_signdist(data, reg)
  return(0)
}

plot_signdist <- function(data, reg) {
  par(mar = c(4, 4, 7, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Age versus Sight Distance\n", coefs["(Intercept)"], coefs["Age"],
    "Age"
  )
  plot(data$Age, data$Distance,
    main = main_label, xlab = "Age", ylab = "Sight Distance",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  box(which = "plot", lty = "solid")
  summ <- summary(reg)
  legends <- c(paste0("S - ", format(summ$sigma, digits = 4)), paste0(
    "Rsq - ",
    format(summ$r.squared, digits = 4)
  ), paste0("Rsq(adj) - ", format(summ$adj.r.squared,
    digits = 4
  )))
  legend("topright", legends)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

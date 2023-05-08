#!/usr/bin/env Rscript
alligator.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/alligator.txt")
}
library(skimr)

main <- function(argv) {
  data <- read.table(alligator.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  process_alligator(data)

  return(0)
}

process_alligator <- function(data) {
  reg <- lm(weight ~ length, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_alligator(data, reg)
}

plot_alligator <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste("Length versus Weight\n", coefs["(Intercept)"], x <- if (sign(coefs["length"]) ==
    1) {
    "+"
  } else {
    "-"
  }, abs(coefs["length"]), "Length")
  plot(data$length, data$weight,
    main = main_label, xlab = "Length", ylab = "Weight",
    pch = 19, frame = TRUE
  )
  abline(reg, col = "blue")
  summ <- summary(reg)
  legends <- c(paste0("S - ", format(summ$sigma, digits = 4)), paste0(
    "Rsq - ",
    format(summ$r.squared, digits = 4)
  ), paste0("Rsq(adj) - ", format(summ$adj.r.squared,
    digits = 4
  )))
  legend("topleft", legends)
  abline(h = mean(data$weight), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

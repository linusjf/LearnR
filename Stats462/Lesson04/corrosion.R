#!/usr/bin/env Rscript
corrosion.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/corrosion.txt")
}
library(skimr)

main <- function(argv) {
  data <- read.table(corrosion.txt(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))

  process_corrosion(data)

  return(0)
}

process_corrosion <- function(data) {
  reg <- lm(wgtloss ~ iron, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_corrosion(data, reg)
}

plot_corrosion <- function(data, reg) {
  par(mar = c(4, 4, 5, 1))
  coefs <- reg$coefficients
  main_label <- paste("Iron versus Weight Loss\n", coefs["(Intercept)"], x <- if (sign(coefs["iron"]) == 
    1) {
    "+"
  } else {
    "-"
  }, abs(coefs["iron"]), "Iron")
  plot(data$iron, data$wgtloss, main = main_label, xlab = "Iron", ylab = "Weight Loss", 
    pch = 19, frame = TRUE)
  abline(reg, col = "blue")
  summ <- summary(reg)
  legends <- c(paste0("S - ", format(summ$sigma, digits = 4)), paste0("Rsq - ", 
    format(summ$r.squared, digits = 4)), paste0("Rsq(adj) - ", format(summ$adj.r.squared, 
    digits = 4)))
  legend("topright", legends)
  abline(h = mean(data$wgtloss), col = "black", lty = "dashed")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

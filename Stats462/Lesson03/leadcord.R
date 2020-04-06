#!/usr/bin/env Rscript
leadcord() <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/leadcord.txt"
  )
}
library(skimr)

main <- function(argv) {
  data <- read.table(leadcord(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Cord ~ Sold, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_leadcord(data, reg)
  return(0)
}

plot_leadcord <- function(data, reg) {
  par(mar = c(4, 4, 7, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "monthly gasoline
                       lead sales (in metric tons) versus mean lead
    concentrations (µl/dl) in umbilical cord blood of babies\n",
    coefs["(Intercept)"],
    "+",
    coefs["Sold"], "Sold"
  )
  plot(data$Sold, data$Cord,
    main = main_label,
    xlab = "Sold", ylab = "Cord",
    pch = 19, frame = FALSE
  )
  abline(reg, col = "blue")
  box(which = "plot", lty = "solid")
  summ <- summary(reg)
  legends <- c(
    paste0("S - ", format(summ$sigma, digits = 4)),
    paste0("Rsq - ", format(summ$r.squared, digits = 4)),
    paste0("Rsq(adj) - ", format(summ$adj.r.squared, digits = 4))
  )
  legend("bottomright", legends)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

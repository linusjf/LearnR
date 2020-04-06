#!/usr/bin/env Rscript
handsheight() <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/handsheight.txt"
  )
}
library(skimr)

main <- function(argv) {
  data <- read.table(handsheight(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Height ~ HandSpan, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_handheight(data, reg)
  return(0)
}

plot_handheight <- function(data, reg) {
  par(mar = c(4, 4, 7, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "HandSpan versus Height\n",
    coefs["(Intercept)"],
    "+",
    coefs["HandSpan"], "HandSpan"
  )
  plot(data$HandSpan, data$Height,
    main = main_label,
    xlab = "HandSpan", ylab = "Height",
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

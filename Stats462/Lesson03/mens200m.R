#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("mens200m.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Men200m ~ Year, data = data)
  print(reg)
  print(summary(reg))
  print(confint(reg))
  plot_mens200m(data, reg)
  return(0)
}

plot_mens200m <- function(data, reg) {
  par(mar = c(4, 4, 7, 1))
  coefs <- reg$coefficients
  main_label <- paste(
    "Year versus Men's 200 m timings\n",
                      coefs["(Intercept)"],
    coefs["Year"], "Year"
  )
  plot(data$Year, data$Men200m,
    main = main_label,
    xlab = "Year", ylab = "Men's 200m timings",
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
  legend("bottomleft", legends)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

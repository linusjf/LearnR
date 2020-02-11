#!/usr/bin/env Rscript
suppressMessages(library(RGraphics))

main <- function(argv) {
  print(head(pressure))
  pdf("simplescatter.pdf")
  plot(pressure)
  text(
    150, 600,
    "Pressure (mm Hg)\nversus\nTemperature (Celsius)"
  )
  print(dev.cur())
  graphics.off()
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript
# Comment: A slightly modified version of Figure 1.1 from Cleveland's book
# 'Visualizing Data'
suppressMessages(library(lattice))

main <- function(argv) {
  print(head(lattice::barley))
  print(levels(lattice::barley$variety))
  print(levels(lattice::barley$year))
  print(levels(lattice::barley$site))
  lattice::trellis.device(pdf, file = "dotplot.pdf")
  trellis.par.set(list(
    fontsize = list(text = 6), par.xlab.text = list(cex = 1.5),
    add.text = list(cex = 1.5), superpose.symbol = list(cex = 0.5)
  ))
  key <- lattice::simpleKey(levels(lattice::barley$year), space = "right")
  key$text$cex <- 1.5
  print(dotplot(variety ~ yield | site,
    data = barley, groups = year, key = key,
    xlab = "Barley Yield (bushels/acre) ", aspect = 0.5, layout = c(1, 6), ylab = NULL
  ))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

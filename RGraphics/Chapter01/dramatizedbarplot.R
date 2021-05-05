#!/usr/bin/env Rscript
library(colorspace)
library(grid)
suppressMessages(library(grImport))

main <- function(argv) {
  # Produce a plot of tiger populations with picture as background Source:
  # http://www.globaltiger.org/population.htm
  year <- c(1993, 1996, 1998, 2001)
  # nolint start minpop <- c(20, 50, 50, 115) nolint end
  maxpop <- c(50, 240, 240, 150)

  tiger <- readPicture(system.file("extra", "tiger.ps.xml", package = "RGraphics"))[-1]

  source(system.file("extra", "grayify.R", package = "RGraphics"))

  # nolint start
  pushViewport(plotViewport(c(3, 2, 2, 1)), viewport(xscale = c(1991, 2003), yscale = c(0, 
    250)))
  grid::grid.rect()
  # tiger backdrop in gray
  grImport::grid.picture(tiger, x = 0.45, FUN = grayify, min = 0.8)
  # nolint end
  grid::grid.xaxis(at = year, gp = grid::gpar(cex = 0.7))
  grid::grid.yaxis(gp = grid::gpar(cex = 0.7))
  # black bars
  grid::grid.rect(x = unit(year, "native"), y = 0, width = unit(1, "native"), height = unit(maxpop, 
    "native"), just = "bottom", gp = grid::gpar(fill = "black"))
  # tiger in bars
  tiger_grob <- pictureGrob(tiger, x = 0.45, FUN = grobify)
  # Start from 2 because bar 1 does not overlap with tiger
  for (i in 2:length(year)) {
    grid::grid.clip(x = unit(year[i], "native"), y = 0, width = unit(1, "native"), 
      height = unit(maxpop[i], "native"), just = "bottom")
    # tiger backdrop (shift slightly to left so get one eye in one bar)
    grid::grid.draw(tiger_grob)
  }
  grid::grid.clip()
  # redo bar borders
  grid::grid.rect(x = unit(year, "native"), y = 0, width = unit(1, "native"), height = unit(maxpop, 
    "native"), just = "bottom", gp = grid::gpar(fill = NA))
  grid::grid.text("Estimated Population (max.) of Bengal Tigers\n(in
                      Bhutan)", 
    y = unit(1, "npc") + unit(1, "lines"))
  grid::popViewport(2)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

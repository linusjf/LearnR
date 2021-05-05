#!/usr/bin/env Rscript
# Comment: A bit of mucking around is required to get the second (whole-world)
# map positioned correctly; this provides an example of calling a plotting
# function to perform calculations but do no drawing (see the second call to the
# map() function).  Makes use of the 'maps', 'mapdata', and 'mapproj' packages to
# draw the maps.
library(maps)
library(mapdata)

main <- function(argv) {
  par(mar = rep(1, 4))
  map("nzHires", fill = TRUE, col = "gray80", regions = c("North Island", "South Island", 
    "Stewart Island"))
  points(174.75, -36.87, pch = 16, cex = 2, col = rgb(0, 0, 0, 0.5))
  arrows(172, -36.87, 174, -36.87, lwd = 3)
  text(172, -36.87, "Auckland  ", adj = 1, cex = 2)
  # mini world map as guide
  maplocs <- map(projection = "sp_mercator", wrap = TRUE, lwd = 0.1, col = "gray", 
    ylim = c(-60, 75), interior = FALSE, orientation = c(90, 180, 0), add = TRUE, 
    plot = FALSE)
  xrange <- range(maplocs$x, na.rm = TRUE)
  yrange <- range(maplocs$y, na.rm = TRUE)
  aspect <- abs(diff(yrange))/abs(diff(xrange))
  # customised to 6.5 by 4.5 figure size
  par(fig = c(0.99 - 0.5, 0.99, 0.01, 0.01 + 0.5 * aspect * 4.5/6.5), mar = rep(0, 
    4), new = TRUE)
  plot.new()
  plot.window(xlim = xrange, ylim = yrange)
  map(projection = "sp_mercator", wrap = TRUE, lwd = 0.5, ylim = c(-60, 75), interior = FALSE, 
    orientation = c(90, 180, 0), add = TRUE)
  symbols(-0.13, -0.8, circles = 1, inches = 0.1, add = TRUE)
  box()
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

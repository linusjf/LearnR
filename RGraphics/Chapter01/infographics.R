#!/usr/bin/env Rscript
library(pixmap)
library(grid)

main <- function(argv) {
  pic <- read.pnm(system.file("extra", "AfterTheBombs.pnm", package = "RGraphics"))

  source(system.file("extra", "as.raster.R", package = "RGraphics"))

  pic_raster <- as.raster(pic)
  bg <- pic_raster

  unknown <- 8.7
  total <- 9.1

  theta0 <- pi/4
  theta_n <- theta0 + 2 * pi * unknown/total
  theta <- seq(theta0, theta_n, length.out = 100)
  x <- 0.3 * c(0, cos(theta)) + 0.5
  y <- 0.3 * c(0, sin(theta)) + 0.35

  grid::grid.raster(bg)
  pushViewport(viewport(width = unit(1, "snpc"), height = unit(1, "snpc"), gp = gpar(cex = 1.2)))
  grid::grid.polygon(x, y, gp = grid::gpar(col = NA, fill = rgb(0.67, 0, 0.11, 
    0.7)))
  label1 <- textGrob("UNACCOUNTED\nFOR", unit(0.2, "npc") - unit(2, "mm"), unit(0.6, 
    "npc") + unit(2, "mm"), gp = grid::gpar(cex = 1.4, fontface = "bold"), just = c("right", 
    "bottom"))
  grid.rect(0.2, 0.6, just = c("right", "bottom"), width = grobWidth(label1) + 
    unit(4, "mm"), height = grobHeight(label1) + unit(4, "mm"), gp = grid::gpar(col = NA, 
    fill = rgb(1, 1, 1, 0.5)))
  grid::grid.draw(label1)
  label2 <- textGrob("ACCOUNTED\nFOR", unit(0.8, "npc") + unit(2, "mm"), unit(0.6, 
    "npc") + unit(2, "mm"), gp = grid::gpar(cex = 1.4, fontface = "bold"), just = c("left", 
    "bottom"))
  grid.rect(0.8, 0.6, just = c("left", "bottom"), width = grobWidth(label2) + unit(4, 
    "mm"), height = grobHeight(label2) + unit(4, "mm"), gp = gpar(col = NA, fill = rgb(1, 
    1, 1, 0.5)))
  grid::grid.draw(label2)
  grid::grid.segments(c(0.2, 0.8), 0.6, c(0.3, 0.7), 0.5, gp = gpar(lty = "dotted", 
    lwd = 2))
  heading <- textGrob("The Department of Defense is unable to account for the
                      use of
$8.7 billion of the $9.1 billion it spent on reconstruction in Iraq", 
    x = unit(0.5, "cm"), y = unit(3, "lines"), just = c("left", "top"), gp = gpar(cex = 1, 
      col = "white"))
  pushViewport(viewport(x = 0, y = 1, just = c("left", "top"), height = grobHeight(heading) + 
    unit(4, "lines"), width = grobWidth(heading) + unit(1, "cm")))
  grid::grid.rect(gp = grid::gpar(fill = "black"))
  grid.segments(x0 = unit(0.5, "cm"), x1 = unit(1, "npc") - unit(0.5, "cm"), y0 = unit(1, 
    "npc") - unit(2, "lines"), y1 = unit(1, "npc") - unit(2, "lines"), gp = grid::gpar(col = "grey50", 
    lwd = 2))
  grid.text("That's 96 Percent", x = unit(0.5, "cm"), y = unit(1, "npc") - unit(1, 
    "lines"), just = "left", gp = grid::gpar(fontface = "bold", col = "white"))
  grid::grid.draw(heading)
  grid::popViewport(2)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

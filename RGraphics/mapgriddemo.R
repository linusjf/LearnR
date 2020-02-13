#!/usr/bin/env Rscript
library(maps)
library(mapproj)

main <- function(argv) {
  m <- maps::map("usa", plot = FALSE)
  maps::map("usa", project = "albers", par = c(39, 45))
  mapproj::map.grid(m)
  # get unprojected world limits
  m <- maps::map("world", plot = FALSE)
  # center on NYC
  maps::map("world", proj = "azequalarea", orient = c(41, -74, 0))
  mapproj::map.grid(m, col = 2)
  points(
    mapproj::mapproject(
      list(
        y = 41,
        x = -74
      )
    ),
    col = 3,
    pch = "x",
    cex = 2
  )
  maps::map("world", proj = "orth", orient = c(41, -74, 0))
  mapproj::map.grid(m, col = 2, nx = 6, ny = 5, label = FALSE, lty = 2)
  points(
    mapproj::mapproject(
      list(
        y = 41,
        x = -74
      )
    ),
    col = 3,
    pch = "x",
    cex = 2
  )
  # center on Auckland
  maps::map("world", proj = "orth", orient = c(-36.92, 174.6, 0))
  mapproj::map.grid(m, col = 2, label = FALSE, lty = 2)
  points(
    mapproj::mapproject(
      list(
        y = -36.92,
        x = 174.6
      )
    ),
    col = 3,
    pch = "x",
    cex = 2
  )
  m <- maps::map("nz")
  # center on Auckland
  maps::map("nz", proj = "azequalarea", orient = c(-36.92, 174.6, 0))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

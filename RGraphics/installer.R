#!/usr/bin/env Rscript

install_rgraphics <- function() {
  if (!require(RGraphics)) {
    install.packages("RGraphics", .libPaths()[1])
  }
  if (!require(R.devices)) {
    install.packages("R.devices", .libPaths()[1])
  }
  if (!require(colorspace)) {
    install.packages("colorspace", .libPaths()[1])
  }
  if (!require(grImport)) {
    install.packages("grImport", .libPaths()[1])
  }
  if (!require(grid)) {
    install.packages("grid", .libPaths()[1])
  }
  if (!require(lattice)) {
    install.packages("lattice", .libPaths()[1])
  }
  if (!require(ggplot2)) {
    install.packages("ggplot2", .libPaths()[1])
  }
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  if (!require(graph)) {
    BiocManager::install(c("graph"))
  }
  if (!require(pixmap)) {
    install.packages("pixmap")
  }
}

main <- function(argv) {
  print(Sys.info())
  sess <- sessionInfo()
  print(sess)
  platform <- ifelse(is.null(sess$running), "termux", sess$running)
  switch(platform,
    "termux" = {
      Sys.setenv(R_LIBS_USER = "/data/data/com.termux/files/usr/lib/R/library")
    },
    `Arch Linux ARM` = {
      Sys.setenv(R_LIBS_USER = "/usr/lib/R/library")
    }
  )
  install_rgraphics()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}
# nolint end

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}

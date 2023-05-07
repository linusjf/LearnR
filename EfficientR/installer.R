#!/usr/bin/env Rscript

install_efficientr <- function() {
  if (!require(microbenchmark)) {
    install.packages("microbenchmark", .libPaths()[1])
  }
  if (!require(profvis)) {
    install.packages("profvis", .libPaths()[1])
  }
  if (!require(ggplot2)) {
    install.packages("ggplot2", .libPaths()[1])
  }
  if (!require(rnoaa)) {
    install.packages("rnoaa", .libPaths()[1])
  }
  if (!require(raster)) {
    install.packages("raster", .libPaths()[1])
  }
  if (!require(benchmarkme)) {
    install.packages("benchmarkme", .libPaths()[1])
  }
  if (!require(assertive.reflection)) {
    install.packages("assertive.reflection", .libPaths()[1])
  }
  if (!require(fortunes)) {
    install.packages("fortunes", .libPaths()[1])
  }
  if (!require(compiler)) {
    install.packages("compiler", .libPaths()[1])
  }
  if (!require(memoise)) {
    install.packages("memoise", .libPaths()[1])
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
  install_efficientr()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}

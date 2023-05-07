#!/usr/bin/env Rscript

install_programs <- function() {
  if (!require(stargazer)) {
    install.packages("stargazer", .libPaths()[1])
  }
  if (!require(compiler)) {
    install.packages("compiler", .libPaths()[1])
  }
  if (!require(rbenchmark)) {
    install.packages("rbenchmark", .libPaths()[1])
  }
  if (!require(pixmap)) {
    install.packages("pixmap", .libPaths()[1])
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
  install_programs()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}
# nolint end

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}

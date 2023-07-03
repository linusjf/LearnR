#!/usr/bin/env Rscript

install_programs <- function() {
  if (!require(tsibble)) {
    install.packages("tsibble", .libPaths()[1])
  }
  if (!require(tsibbledata)) {
    install.packages("tsibbledata", .libPaths()[1])
  }
  if (!require(readr)) {
    install.packages("readr", .libPaths()[1])
  }
  if (!require(utils)) {
    install.packages("utils", .libPaths()[1])
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

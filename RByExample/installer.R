#!/usr/bin/env Rscript

install_rbyexample <- function() {
  if (!require(stringi)) {
    install.packages("stringi", .libPaths()[1])
  }
  if (!require(xtable)) {
    install.packages("xtable", .libPaths()[1])
  }
  if (!require(tseries)) {
    install.packages("tseries", .libPaths()[1])
  }
  if (!require(Hmisc)) {
    install.packages("Hmisc", .libPaths()[1])
  }
  if (!require(foreign)) {
    install.packages("foreign", .libPaths()[1])
  }
  if (!require(StatDataML)) {
    install.packages("StatDataML", .libPaths()[1])
  }
  if (!require(zoo)) {
    install.packages("zoo", .libPaths()[1])
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
  install_rbyexample()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}

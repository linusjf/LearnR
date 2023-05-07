#!/usr/bin/env Rscript

install_rethinking <- function() {
  if (!require(rstan)) {
    install.packages("rstan", .libPaths()[1])
  }
  if (!require(coda)) {
  install.packages("coda", .libPaths()[1])
  }
  if (!require(mvtnorm)) {
  install.packages("mvtnorm", .libPaths()[1])
  }
  if (!require(devtools)) {
  install.packages("devtools", .libPaths()[1])
  }
  if (!require(loo)) {
  install.packages("loo", .libPaths()[1])
  }
  if (!require(dagitty)) {
  install.packages("dagitty", .libPaths()[1])
  }
  devtools::install_github("rmcelreath/rethinking")
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
  install_rethinking()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}

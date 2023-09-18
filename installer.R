#!/usr/bin/env Rscript

install_tools <- function() {
  if (!require(lintr)) {
    install.packages("lintr", .libPaths()[1])
  }
  if (!require(styler)) {
    install.packages("styler", .libPaths()[1])
  }
  if (!require(formatR)) {
    install.packages("formatR", .libPaths()[1])
  }
  if (!require(parallel)) {
    install.packages("parallel", .libPaths()[1])
  }
  if (!require(rex)) {
    install.packages("rex", .libPaths()[1])
  }
  if (!require(devtools)) {
    install.packages("devtools", .libPaths()[1])
  }
  if (!require(roxygen2)) {
    install.packages("roxygen2", .libPaths()[1])
  }
  if (!require(startup)) {
    install.packages("startup", .libPaths()[1])
  }
  if (!require(rprojroot)) {
    install.packages("rprojroot", .libPaths()[1])
  }
  if (!require(githubinstall)) {
    install.packages("githubinstall", .libPaths()[1])
  }
  if (!require(pacman)) {
    install.packages("pacman", .libPaths()[1])
  }
  if (!require(drat)) {
    install.packages("drat", .libPaths()[1])
  }
  if (!require(rmarkdown)) {
    install.packages("rmarkdown", .libPaths()[1])
  }
  if (!require(quarto)) {
    install.packages("quarto", .libPaths()[1])
  }
  if (!require(rprofile)) {
    install.packages("rprofile", .libPaths()[1])
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
  install_tools()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}

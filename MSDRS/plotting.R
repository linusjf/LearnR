#!/usr/bin/env Rscript
library(titanic)
data("titanic_train", package = "titanic")
library(faraway)
data("worldcup")
library(ggplot2)

main <- function(argv) {
  titanic <- titanic_train

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

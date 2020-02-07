#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(ggplot2)
suppressMessages(library(gridExtra))
library(ggthemes)
library(faraway)
data(nepali)
data(worldcup)
suppressMessages(library(dlnm))
data(chicagoNMMAPS)

main <- function(argv) {
  chic <- dlnm::chicagoNMMAPS
  print(head(chic))
  chic_july <- chic %>%
    filter(month == 7 & year == 1995)
  print(head(chic_july))
  plots <- list()
  plots[[1]] <- ggplot(worldcup, aes(x = Time, y = Shots)) +
    geom_point() +
    theme_classic()
  plots[[2]] <- ggplot(worldcup, aes(x = Time, y = Shots)) +
    geom_point() +
    theme_tufte()
  pdf("customplots.pdf")
  invisible(lapply(plots, print))
  graphics.off()
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

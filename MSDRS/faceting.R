#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(faraway)
data(worldcup)
library(ggplot2)
library(tidyr)

main <- function(argv) {
  worldcup %>%
    select(Position, Time, Shots, Tackles, Saves) %>%
    gather(Type, Number, -Position, -Time) %>%
    ggplot(aes(x = Time, y = Number)) +
    geom_point() +
    facet_grid(Type ~ Position)
  ggplot2::ggsave(file = "faceted.pdf")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

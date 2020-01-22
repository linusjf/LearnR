#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(faraway)
data(worldcup)

main <- function(argv) {
  worldcup <- dplyr::mutate(worldcup, Player_Name = rownames(worldcup))
  print(dplyr::slice(worldcup, 1:3))

  worldcup <- worldcup %>%
    group_by(Position) %>%
    mutate(Ave_Shots = mean(Shots)) %>%
    ungroup()
  print(dplyr::slice(worldcup, 1:3))
  avgshots <- worldcup %>%
    group_by(Position) %>%
    summarize(Ave_Shots = mean(Shots)) %>%
    ungroup()
  print(dplyr::slice(avgshots, 1:3))

  print(worldcup %>%
    rename(Name = Player_Name) %>%
    slice(1:3))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

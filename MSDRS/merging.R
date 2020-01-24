#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(faraway)
library(knitr)
data(worldcup)
library(readr)

main <- function(argv) {
  team_standings <- readr::read_csv("team_standings.csv")
  print(dplyr::slice(team_standings, 1:3))
  print(worldcup %>%
    mutate(
      Name = rownames(worldcup),
      Team = as.character(Team)
    ) %>%
    select(Name, Position, Shots, Team) %>%
    arrange(desc(Shots)) %>%
    slice(1:5) %>% # Merge in team standings
    left_join(team_standings, by = "Team") %>%
    rename("Team Standing" = Standing) %>%
    kable())
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

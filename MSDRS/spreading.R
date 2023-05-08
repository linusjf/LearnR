#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(faraway)
data(worldcup)
library(knitr)
library(tidyr)

main <- function(argv) {
  # Summarize the data to create the summary statistics you want
  wc_table <- worldcup %>%
    filter(Team %in% c("Spain", "Netherlands", "Uruguay", "Germany")) %>%
    select(Team, Position, Passes) %>%
    group_by(Team, Position) %>%
    summarize(
      ave_passes = mean(Passes), min_passes = min(Passes), max_passes = max(Passes),
      pass_summary = paste0(
        round(ave_passes), " (", min_passes, ", ", max_passes,
        ")"
      )
    ) %>%
    select(Team, Position, pass_summary)
  # What the data looks like before using `spread`
  print(wc_table)

  # Use spread to create a prettier format for a table
  print(wc_table %>%
    spread(Position, pass_summary) %>%
    kable())

  # Summarize the data to create the summary statistics you want
  wc_table <- worldcup %>%
    select(Team, Position, Passes) %>%
    filter(Position == "Goalkeeper") %>%
    group_by(Team, Position) %>%
    summarize(ave_passes = mean(Passes), min_passes = min(Passes), max_passes = max(Passes)) %>%
    select(Team, Position, ave_passes, min_passes, max_passes) %>%
    filter(min_passes != max_passes)

  # What the data looks like before using `spread`
  print(wc_table)

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

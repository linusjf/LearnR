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
library(forcats)

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
  chicago_plot <- ggplot(chic_july, aes(x = date, y = death)) +
    xlab("Day in July 1995") +
    ylab("All-cause deaths") +
    ylim(0, 450)
  plots[[3]] <- chicago_plot +
    geom_point(color = "red") +
    theme_get()
  plots[[4]] <- chicago_plot +
    geom_point(color = "red") +
    theme_bw()
  plots[[5]] <- chicago_plot +
    geom_point(color = "red") +
    theme_few()
  plots[[6]] <- chicago_plot +
    geom_point(color = "red") +
    theme_tufte()
  plots[[7]] <- chicago_plot +
    geom_point(color = "red") +
    theme_fivethirtyeight()
  plots[[8]] <- chicago_plot +
    geom_point(color = "red") +
    theme_solarized()
  plots[[9]] <- chicago_plot +
    geom_area(fill = "black") +
    theme_excel()
  plots[[10]] <- chicago_plot +
    geom_line() +
    theme_tufte()

  # Create a messier example version of the data
  wc_example_data <- worldcup %>%
    dplyr::rename(Pos = Position) %>%
    mutate(Pos = fct_recode(Pos,
      "DC" = "Defender",
      "FW" = "Forward",
      "GK" = "Goalkeeper",
      "MF" = "Midfielder"
    ))
  plots[[11]] <- ggplot(
    wc_example_data,
    aes(x = Pos)
  ) +
    geom_bar()

  wc_example_data <- wc_example_data %>%
    mutate(Pos = fct_recode(Pos,
      "Defender" = "DC",
      "Forward" = "FW",
      "Goalkeeper" = "GK",
      "Midfielder" = "MF"
    ))
  plots[[12]] <- ggplot(wc_example_data, aes(x = Pos)) +
    geom_bar(fill = "lightgray") +
    xlab("") +
    ylab("Number of players") +
    coord_flip() +
    theme_tufte()
  worldcup_forwards <-
    filter(
      worldcup,
      Position == "Forward"
    )
  plots[[13]] <-
    ggplot(
      worldcup_forwards,
      aes(
        x = Passes,
        y = Shots
      )
    ) +
    geom_point(size = 1.5) +
    theme_few() +
    geom_smooth()
  plots[[14]] <-
    ggplot(
      worldcup_forwards,
      aes(
        x = Passes,
        y = Shots
      )
    ) +
    geom_point(size = 1.5) +
    theme_few() +
    geom_smooth(method = "lm")

  plots[[15]] <- ggplot(worldcup, aes(x = Time, y = Shots)) +
    geom_point() +
    facet_grid(. ~ Position)
  worldcup_finalists <- worldcup %>%
    filter(Team %in% c("Spain", "Netherlands"))
  plots[[16]] <- ggplot(worldcup_finalists, aes(x = Time, y = Shots)) +
    geom_point() +
    facet_grid(Team ~ Position)
  plots[[17]] <- ggplot(worldcup, aes(x = Time, y = Shots)) +
    geom_point(alpha = 0.25) +
    facet_wrap(~Team, ncol = 6)
  pdf("customplots.pdf")
  invisible(lapply(plots, print))
  graphics.off()
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

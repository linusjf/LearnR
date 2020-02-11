#!/usr/bin/env Rscript
suppressMessages(library(dlnm))
data(chicagoNMMAPS)
suppressMessages(library(dplyr))
library(faraway)
data(nepali)
data(worldcup)
library(forcats)
library(ggplot2)
library(ggthemes)
library(grid)
suppressMessages(library(gridExtra))
library(RColorBrewer)
suppressMessages(library(viridis))

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
  nepali <- nepali %>%
    mutate(
      sex = factor(sex,
        levels = c(1, 2),
        labels = c("Male", "Female")
      )
    )
  plots[[18]] <- ggplot(nepali, aes(ht, wt)) +
    geom_point() +
    facet_grid(. ~ sex)
  nepali <- nepali %>%
    mutate(
      sex = factor(sex,
        levels = c("Female", "Male")
      )
    )
  plots[[19]] <- ggplot(nepali, aes(ht, wt)) +
    geom_point() +
    facet_grid(. ~ sex)

  ## Left plot
  worldcup_mean_times <- worldcup %>%
    group_by(Team) %>%
    summarize(mean_time = mean(Time))
  plots[[20]] <- ggplot(
    worldcup_mean_times,
    aes(x = mean_time, y = Team)
  ) +
    geom_point() +
    theme_few() +
    xlab("Mean time per player (minutes)") +
    ylab("")

  ## Right plot
  worldcup_mean_times <- worldcup %>%
    group_by(Team) %>%
    summarize(mean_time = mean(Time)) %>%
    arrange(mean_time) %>% # re-order and re-set
    mutate(Team = factor(Team, levels = Team))
  # factor levels before plotting
  plots[[21]] <- ggplot(
    worldcup_mean_times,
    aes(x = mean_time, y = Team)
  ) +
    geom_point() +
    theme_few() +
    xlab("Mean time per player (minutes)") +
    ylab("")
  noteworthy_players <- worldcup %>%
    filter(Shots == max(Shots) | Passes == max(Passes)) %>%
    mutate(point_label = paste0(Team, Position, sep = ", "))
  plots[[22]] <- ggplot(worldcup, aes(x = Passes, y = Shots)) +
    geom_point(alpha = 0.5) +
    geom_text(
      data = noteworthy_players, aes(label = point_label),
      vjust = "inward", hjust = "inward", color = "blue"
    ) +
    theme_few()
  plots[[23]] <- worldcup %>%
    ggplot(aes(x = Time, y = Shots, color = Position)) +
    geom_point()
  plots[[24]] <- worldcup %>%
    ggplot(aes(x = Time, y = Shots)) +
    geom_point() +
    facet_grid(. ~ Position)
  plots[[25]] <- worldcup %>%
    dplyr::select(Team, Time) %>%
    dplyr::group_by(Team) %>%
    dplyr::mutate(
      ave_time = mean(Time),
      min_time = min(Time),
      max_time = max(Time)
    ) %>%
    dplyr::arrange(ave_time) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Team = factor(Team, levels = unique(Team))) %>%
    ggplot(aes(x = Time, y = Team)) +
    geom_segment(aes(x = min_time, xend = max_time, yend = Team),
      alpha = 0.5, color = "gray"
    ) +
    geom_point(alpha = 0.5) +
    geom_point(aes(x = ave_time), size = 2, color = "red", alpha = 0.5) +
    theme_minimal() +
    ylab("")
  plots[[26]] <-
    ggplot(
      worldcup,
      aes(
        x = Time, y = Passes,
        color = Position,
        size = Shots
      )
    ) +
    geom_point(alpha = 0.5)
  plots[[27]] <-
    ggplot(
      worldcup,
      aes(
        x = Time,
        y = Passes,
        color = Position,
        size = Shots
      )
    ) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(
      name = "Time played (minutes)",
      breaks = 90 * c(2, 4, 6),
      minor_breaks = 90 * c(1, 3, 5)
    )
  plots[[27]] <-
    ggplot(
      worldcup,
      aes(
        x = Time,
        y = Passes,
        color = Position,
        size = Shots
      )
    ) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(
      name = "Time played (minutes)",
      breaks = 90 * c(2, 4, 6),
      minor_breaks = 90 * c(1, 3, 5)
    ) +
    scale_size_continuous(
      name = "Shots on goal",
      breaks = c(0, 10, 20)
    )
  plots[[28]] <-
    ggplot(chic_july, aes(x = date, y = death)) +
    geom_line() +
    scale_x_date(
      name = "Date in July 1995",
      date_labels = "%m-%d"
    )
  plots[[29]] <-
    ggplot(chic_july, aes(x = date, y = death)) +
    geom_line() +
    scale_x_date(
      name = "Date in July 1995",
      date_labels = "%b-%d"
    ) +
    scale_y_log10(breaks = c(1:4 * 100))
  wc_example <- ggplot(worldcup, aes(
    x = Time, y = Passes,
    color = Position, size = Shots
  )) +
    geom_point(alpha = 0.5)
  a <- wc_example +
    scale_color_brewer(palette = "Set1") +
    ggtitle("Set1")
  b <- wc_example +
    scale_color_brewer(palette = "Dark2") +
    ggtitle("Dark2")
  c <- wc_example +
    scale_color_brewer(palette = "Pastel2") +
    ggtitle("Pastel2") +
    theme_dark()
  d <- wc_example +
    scale_color_brewer(palette = "Accent") +
    ggtitle("Accent")
  grob <-
    gridExtra::grid.arrange(a, b, c, d, ncol = 2)
  newplots <- list()
  newplots[[1]] <- ggplot(worldcup, aes(
    x = Time, y = Passes,
    color = Position, size = Shots
  )) +
    geom_point(alpha = 0.5) +
    scale_color_manual(
      values =
        c(
          "blue",
          "red",
          "darkgreen",
          "darkgray"
        )
    )
  newplots[[2]] <- worldcup %>%
    ggplot(aes(x = Time, y = Shots, color = Passes)) +
    geom_point(size = 0.9) +
    facet_wrap(~Position) +
    scale_color_viridis()
  newplots[[3]] <- worldcup %>%
    ggplot(aes(x = Time, y = Shots, color = Position)) +
    geom_point(alpha = 0.7) +
    scale_color_viridis(discrete = TRUE)
  worldcup_ex <- worldcup %>%
    ggplot(aes(x = Time, y = Shots, color = Passes)) +
    geom_point(size = 0.9)

  magma_plot <- worldcup_ex +
    scale_color_viridis(option = "A") +
    ggtitle("magma")
  inferno_plot <- worldcup_ex +
    scale_color_viridis(option = "B") +
    ggtitle("inferno")
  plasma_plot <- worldcup_ex +
    scale_color_viridis(option = "C") +
    ggtitle("plasma")
  viridis_plot <- worldcup_ex +
    scale_color_viridis(option = "D") +
    ggtitle("viridis")
  grob_viridis <-
    grid.arrange(magma_plot,
      inferno_plot,
      plasma_plot,
      viridis_plot,
      ncol = 2
    )
  pdf("customplots.pdf")
  invisible(lapply(plots, print))
  RColorBrewer::display.brewer.pal(name = "PuBuGn", n = 8)
  RColorBrewer::display.brewer.pal(name = "Set1", n = 8)
  RColorBrewer::display.brewer.pal(name = "PRGn", n = 8)
  grid::grid.newpage()
  grid::grid.draw(grob)
  invisible(lapply(newplots, print))
  grid::grid.newpage()
  grid::grid.draw(grob_viridis)
  graphics.off()
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

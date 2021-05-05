#!/usr/bin/env Rscript
library(titanic)
data("titanic_train")
library(faraway)
data("worldcup")
library(ggplot2)
suppressMessages(library(dplyr))

main <- function(argv) {
  ggplot(data = titanic_train) + geom_histogram(aes(x = Fare), bins = 15)
  ggplot2::ggsave("titanicfarehist.pdf")
  ggplot(worldcup, aes(x = Time, y = Passes)) + geom_point()
  ggplot2::ggsave("wctimepasses.pdf")
  ggplot(worldcup, aes(x = Time, y = Passes, color = Position, size = Shots)) + 
    geom_point()
  ggplot2::ggsave("wctimepassescolor.pdf")
  noteworthy_players <- worldcup %>%
    filter(Shots == max(Shots) | Passes == max(Passes)) %>%
    mutate(point_label = paste(Team, Position, sep = ", "))
  ggplot(worldcup, aes(x = Passes, y = Shots, color = Position)) + geom_point() + 
    geom_text(data = noteworthy_players, aes(label = point_label), vjust = "inward", 
      hjust = "inward")
  ggplot2::ggsave("wcpassesshotscolor.pdf")
  ggplot(worldcup, aes(x = Time)) + geom_histogram(binwidth = 10) + geom_vline(xintercept = 90 * 
    0:6, color = "blue", alpha = 0.5)
  ggplot2::ggsave("wcplayingtime.pdf")
  ggplot(worldcup, aes(x = Time, y = Passes)) + geom_point(color = "darkgreen")
  ggplot2::ggsave("wctimepassesptscolor.pdf")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))

main <- function(argv) {
  the_caption <- "Source: WHO and many others via Johns Hopkins University.\n
  Analysis by http://freerangestats.info"

  data <- readr::read_csv("rates.csv")

  top_countries <- data %>%
    group_by(country) %>%
    slice(c(n())) %>%
    ungroup() %>%
    top_n(9, wt = confirmed)

  top_countries <- top_countries$country
  remove <- c("World")
  top_countries <-
    top_countries[! top_countries %in% remove]

  print(top_countries)
  #---------------------------global total-------------------
data %<>%
  filter(country %in% top_countries)

  data %>%
    ggplot(aes(x = date, y = rate.upper, color = factor(country))) +
    geom_line() +
    labs(
      caption = the_caption,
      x = "",
      y = "Observed case fatality rate",
      title = "Steadily increasing case fatality rate of COVID-19 in early
      2020",
      subtitle = "Increase probably reflects move of the disease into older
      populations.
Note that actual case fatality is likely to be much lower due to undiagnosed
surviving cases."
    )
    ggplot2::ggsave("cumcfr.pdf")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

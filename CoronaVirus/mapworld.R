#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(readr))
suppressMessages(library(maps))
suppressMessages(library(viridis))

main <- function(argv) {
  # nolint start
  ## get the COVID-19 data
  datafileurl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  # nolint end
  download.file(datafileurl, "confirmed.csv")
  datacov <- readr::read_csv("confirmed.csv",
  skip_empty_rows = TRUE)
  yday <- Sys.Date() - 1
  yday <- format(yday, "%m/%d/%y")
  yday <- gsub("^0", "", yday)
  ydaystring <- gsub("/0", "/", yday)
  yday <- paste0("`", ydaystring, "`")
  print(yday)
 datacov[datacov == 0] <- NA
 datacov <- tidyr::drop_na(datacov, all_of(ydaystring))
 print(nrow(datacov))
  ## get the world map
  world <- ggplot2::map_data("world")

  # cutoffs based on the number of cases
  mybreaks <- c(1, 20, 100, 1000, 50000)

  ggplot() +
    geom_polygon(
      data = world, aes(x = long, y = lat, group = group), fill =
        "grey", alpha = 0.3
    ) +
    geom_point(data = datacov, aes(
      x = Long, y = Lat, size = eval(parse(text = yday)), color =
        eval(parse(text = yday))
    ), stroke = F, alpha = 0.7) +
    scale_size_continuous(
      name = "Cases", trans = "log", range = c(1, 7), breaks = mybreaks,
      labels = c(
        "1-19", "20-99", "100-999", "1,000-49,999",
        "50,000+"
      )
    ) +
    # scale_alpha_continuous(name="Cases", trans="log", range=c(0.1,
    # 0.9),breaks=mybreaks) +
    scale_color_viridis_c(
      option = "inferno", name = "Cases", trans = "log",
      breaks = mybreaks, labels = c(
        "1-19", "20-99",
        "100-999",
        "1,000-49,999",
        "50,000+"
      )
    ) +
    theme_void() +
    guides(colour = guide_legend()) +
    labs(caption = "Data Repository provided by Johns Hopkins CSSE.
         Visualization by DataScience+ ") +
    theme(
      legend.position = "bottom",
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA)
    )
    ggplot2::ggsave("mapworld.pdf")
    return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(httr))
suppressMessages(library(readr))
suppressMessages(library(maps))
suppressMessages(library(viridis))
suppressMessages(library(RCurl))
library(rjson)


download_csv <- function() {
  destination <- "confirmed.csv"
  # assume current directory
  repo <- "https://api.github.com/repos/CSSEGISandData/COVID-19/"
  # nolint start
  path <- "csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  # nolint end
  myopts <- curlOptions(
    useragent = "LearnR",
    ssl.verifypeer = FALSE
  )

  url <-
    getURL(
      paste0(
        repo,
        "commits?path=",
        path
      ),
      useragent = "LearnR", ssl.verifypeer = FALSE
    )
  d <- fromJSON(url)[[1]]
  git_date <- as.POSIXct(d$commit$author$date)
  must_download <- !file.exists(destination) ||
    file.info(destination)$mtime > git_date
  if (must_download) {
    url <- d$url
    commit <- rjson::fromJSON(RCurl::getURL(url, .opts = myopts))
    files <- unlist(lapply(commit$files, "[[", "filename"))
    rawfile <- commit$files[[which(files == path)]]$raw_url
    download.file(rawfile, destination, quiet = TRUE)
    Sys.setFileTime(destination, git_date)
    print("File was downloaded")
  }
}

main <- function(argv) {
  ## get the COVID-19 data
  download_csv()
  datacov <- readr::read_csv("confirmed.csv",
    skip_empty_rows = TRUE
  )
  yday <- Sys.Date() - 1
  yday <- format(yday, "%m/%d/%y")
  yday <- gsub("^0", "", yday)
  ydaystring <- gsub("/0", "/", yday)
  yday <- paste0("`", ydaystring, "`")
  datacov[datacov == 0] <- NA
  datacov <- tidyr::drop_na(
    datacov,
    tidyselect::all_of(ydaystring)
  )
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

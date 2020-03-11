#!/usr/bin/env Rscript

library(magrittr)
suppressMessages(library(lubridate))
library(tidyverse)
suppressMessages(library(gridExtra))
suppressMessages(library(kableExtra))
suppressMessages(library(RCurl))
library(rjson)


download_csv <- function(filename) {
  destination <- filename
  # assume current directory
  repo <- "https://api.github.com/repos/CSSEGISandData/COVID-19/"
  path <- paste0(
    "csse_covid_19_data/csse_covid_19_time_series/",
    filename
  )
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
  d <- rjson::fromJSON(url)[[1]]
  git_date <- as.POSIXct(d$commit$author$date)
  must_download <- !file.exists(destination) |
    file.info(destination)$mtime < git_date
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
  # source data files
  filenames <- c(
    "time_series_19-covid-Confirmed.csv",
    "time_series_19-covid-Deaths.csv",
    "time_series_19-covid-Recovered.csv"
  )
  bin <- lapply(filenames, download_csv)
  ## load data into R
  data_confirmed <- read.csv(filenames[1])
  data_deaths <- read.csv(filenames[2])
  data_recovered <- read.csv(filenames[3])

  print(dim(data_confirmed))
  print(dim(data_deaths))
  print(dim(data_recovered))
  print_sample_data(data_confirmed, "Confirmed")
  return(0)
}

print_sample_data <- function(data, name) {
  tryCatch({
  data[, 1:10] %>%
    sample_n(10) %>%
    kable("latex",
      booktabs = TRUE,
      caption = paste("Raw Data (",
                      name,
                      "First 10 Columns only)"
      )
    ) %>%
    kable_styling(font_size = 6, latex_options = c(
      "striped", "hold_position",
      "repeat_header"
    )) %>%
    landscape() %>%
    save_kable(file = paste0(name,
                             ".pdf"),
    keep_tex = TRUE)
}, error = function(error_condition) {
    return(0)
  })
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

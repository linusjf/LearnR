#!/usr/bin/env Rscript

suppressMessages(library(magrittr))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(gridExtra))
suppressMessages(library(kableExtra))
suppressMessages(library(RCurl))
suppressMessages(library(rjson))


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

## data cleaning and transformation
clean_data <- function(data) {
## remove some columns
data %<>%
  select(-c(Province.State, Lat, Long)) %>%
  rename(country = Country.Region)
## convert from wide to long format
data %<>%
  gather(key = date,
         value = count,
         -country)
## convert from character to date
data %<>%
  mutate(date = date %>%
         substr(2, 8) %>%
         mdy())
## aggregate by country
data %<>%
  group_by(country, date) %>%
  summarise(count = sum(count)) %>%
  as.data.frame()
return(data)
}

main <- function(argv) {
  # source data files
  filenames <- c(
    "time_series_19-covid-Confirmed.csv",
    "time_series_19-covid-Deaths.csv",
    "time_series_19-covid-Recovered.csv"
  )
  lapply(filenames, download_csv)
  ## load data into R
  data_confirmed <- read.csv(filenames[1])
  data_deaths <- read.csv(filenames[2])
  data_recovered <- read.csv(filenames[3])

  print(dim(data_confirmed))
  print(dim(data_deaths))
  print(dim(data_recovered))
  if (FALSE) {
    print_sample_data(data_confirmed, "Confirmed")
    print_sample_data(data_deaths, "Deaths")
    print_sample_data(data_recovered, "Recovered")
  }
  dates <- dates(data_confirmed)
  print(date_range(dates))
  print(latest_date(dates))

  ## clean the three datasets
  data_confirmed %<>%
    clean_data() %>%
    rename(confirmed = count)
  data_deaths %<>%
    clean_data() %>%
    rename(deaths = count)
  data_recovered %<>%
    clean_data() %>%
    rename(recovered = count)
  ## merge above 3 datasets into one, by country and date
  data <- data_confirmed %>%
    merge(data_deaths) %>%
    merge(data_recovered)
  print(data)
  ## counts for the whole world
data_world <- data %>%
  group_by(date) %>%
summarise(country = "World",
confirmed = sum(confirmed),
deaths = sum(deaths),
recovered = sum(recovered))
data %<>%
  rbind(data_world)
## remaining confirmed cases
data %<>%
  mutate(remaining_confirmed = confirmed - deaths - recovered)
print(tail(data))
  return(0)
}

latest_date <- function(dates) {
  return(max(dates))
}

dates <- function(data) {
  cols   <- ncol(data)
  ## get dates from column names
  dates <- names(data)[5:cols] %>%
    substr(2, 8) %>%
    mdy()
  return(dates)
}

date_range <- function(dates) {
 return(range(dates))
}

print_sample_data <- function(data, name) {
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
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

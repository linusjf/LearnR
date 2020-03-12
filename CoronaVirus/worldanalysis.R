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
    gather(
      key = date,
      value = count,
      -country, na.rm = TRUE
    )
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

plot_world_cases <- function(data, top_countries) {
  ## convert from wide to long format, for purpose of drawing a area plot
  data_long <- data %>%
    select(c(country,
             date,
             confirmed,
             remaining.confirmed,
             recovered,
             deaths)) %>%
    gather(
      key = type,
      value = count,
      -c(country, date)
    )
  ## set factor levels to show them in a desirable order
  data_long %<>%
    mutate(
      type =
        factor(
          type,
          c(
            "confirmed",
            "remaining.confirmed", "recovered", "deaths"
          )
        )
    )
  ## cases by type
  df <- data_long %>% filter(country %in% top_countries) %<>%
    mutate(country = country %>% factor(levels = c(top_countries)))
  df %>%
    filter(country != "World") %>%
    ggplot(aes(x = date, y = count, fill = country)) +
    geom_area() +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Cases around the World") +
    theme(legend.title = element_blank()) +
    facet_wrap(~type, ncol = 2, scales = "free_y")
  ggplot2::ggsave("worldcases.pdf")
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
  ## counts for the whole world
  data_world <- data %>%
    group_by(date) %>%
    summarise(
      country = "World",
      confirmed = sum(confirmed),
      deaths = sum(deaths),
      recovered = sum(recovered)
    )
  data %<>%
    rbind(data_world)
  ## remaining confirmed cases
  data %<>%
    mutate(remaining.confirmed = confirmed - deaths - recovered)
  write.csv(data, "summarised.csv")
  data %<>%
    add_rates()
  write.csv(data, "rates.csv")
  plot_top10_confirmed(data)
  return(0)
}

add_rates <- function(data) {

  ## sort by country and date
  data %<>%
    arrange(country, date)
  ## daily increases of deaths and cured cases
  ## set NA to the increases on day1
  day1 <- min(data$date)
  data %<>%
    mutate(
      confirmed.inc =
        ifelse(date == day1,
          NA,
          confirmed - lag(confirmed,
            n = 1
          )
        ),
      deaths.inc =
        ifelse(date == day1,
          NA,
          deaths - lag(deaths, n = 1)
        ),
      recovered.inc =
        ifelse(date == day1,
          NA,
          recovered - lag(
            recovered,
            n = 1
          )
        )
    )
  ## death rate based on total deaths and cured cases
  data %<>%
    mutate(
      rate.upper =
        (100 * deaths /
          (deaths + recovered)) %>%
          round(1)
    )
  ## lower bound: death rate based on total confirmed cases
  data %<>%
    mutate(
      rate.lower =
        (100 * deaths /
          confirmed) %>%
          round(1)
    )
  ## death rate based on the number of death/cured on every single day
  data %<>%
    mutate(
      rate.daily =
        (100 * deaths.inc /
          (deaths.inc +
            recovered.inc)) %>%
          round(1)
    )
  return(data)
}

plot_top10_confirmed <- function(data) {
  max_date <- max(data$date)
  print(max_date)
  ## ranking by confirmed cases
  data_latest <- data %>%
    filter(date == max(date)) %>%
    select(country, date, confirmed, deaths, recovered, remaining.confirmed) %>%
    mutate(ranking = dense_rank(desc(confirmed)))
  ## top 10 countries: 12 incl. 'World' and 'Others'
  top_countries <- data_latest %>%
    filter(ranking <= 12) %>%
    arrange(ranking) %>%
    pull(country) %>%
    as.character()
  ## move 'Others' to the end
  top_countries %<>%
    setdiff("Others") %>%
    c("Others")
  print(top_countries)

  ## put all others in a single group of 'Others'
  df <- data_latest %>%
    filter(!is.na(country) & country != "World") %>%
    mutate(
      country =
        ifelse(ranking <= 12,
          as.character(country), "Others"
        )
    ) %>%
    mutate(country = country %>%
      factor(
        levels =
          c(top_countries)
      ))
  df %<>%
    group_by(country) %>%
    summarise(
      confirmed =
        sum(confirmed)
    )
  ## percentage and label
  df %<>%
    mutate(per = (100 * confirmed /
      sum(confirmed)) %>%
      round(1)) %>%
    mutate(txt = paste0(
      country,
      ": ",
      confirmed,
      " (",
      per,
      "%)"
    ))

  df %>% ggplot(aes(fill = country)) +
    geom_bar(aes(x = "", y = per), stat = "identity") +
    coord_polar("y", start = 0) +
    xlab("") +
    ylab("Percentage (%)") +
    labs(title = paste0(
      "Top 10 Countries with Most Confirmed Cases (",
      max_date, ")"
    )) +
    scale_fill_discrete(name = "Country", labels = df$txt)
  ggplot2::ggsave("top10confirmed.pdf")
  plot_world_cases(data, top_countries)
}

latest_date <- function(dates) {
  return(max(dates))
}

dates <- function(data) {
  cols <- ncol(data)
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
      caption = paste(
        "Raw Data (",
        name,
        "First 10 Columns only)"
      )
    ) %>%
    kable_styling(font_size = 6, latex_options = c(
      "striped", "hold_position",
      "repeat_header"
    )) %>%
    landscape() %>%
    save_kable(
      file = paste0(
        name,
        ".pdf"
      ),
      keep_tex = TRUE
    )
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

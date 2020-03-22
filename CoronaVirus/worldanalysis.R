#!/usr/bin/env Rscript

suppressMessages(library(magrittr))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(gridExtra))
suppressMessages(library(kableExtra))
suppressMessages(library(RCurl))
suppressMessages(library(rjson))
suppressMessages(library("dplyr"))


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
    print(paste("File", rawfile, "was downloaded"))
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

plot_world_cases <- function(data, ready_data) {
  top_countries <- ready_data$top.countries
  max_date <- ready_data$max.date
  ## convert from wide to long format, for purpose of drawing a area plot
  data_long <- data %>%
    select(c(
      country,
      date,
      confirmed,
      remaining.confirmed,
      recovered,
      deaths
    )) %>%
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
  plot1 <- df %>%
    filter(country != "World") %>%
    ggplot(aes(x = date, y = count, fill = country), na.rm = TRUE) +
    geom_area() +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Cases around the World") +
    theme(legend.title = element_blank()) +
    facet_wrap(~type, ncol = 2, scales = "free_y")
  ## excluding Mainland China
  plot2 <- df %>%
    filter(!(country %in% c("World", "Mainland China", "China"))) %>%
    ggplot(aes(x = date, y = count, fill = country), na.rm = TRUE) +
    geom_area() +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Cases around the World (excl. China)") +
    theme(legend.title = element_blank()) +
    facet_wrap(~type, ncol = 2, scales = "free_y")
  ## if India in not in top 10, add it in and remove 'Others'
  if (!("India" %in% top_countries)) {
    top_countries %<>% setdiff("Others") %>%
      c("India")
    df <- data_long %>% filter(country %in% top_countries) %<>%
      mutate(country = country %>% factor(levels = c(top_countries)))
  }
  ## cases by country
  plot3 <- df %>%
    filter(type != "confirmed") %>%
    ggplot(aes(x = date, y = count, fill = type), na.rm = TRUE) +
    geom_area(alpha = 0.5) +
    xlab("Date") +
    ylab("Count") +
    labs(title = paste0("COVID-19 Cases by Country (", max_date, ")")) +
    scale_fill_manual(values = c("red", "green", "black")) +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    facet_wrap(~country, ncol = 3, scales = "free_y")
  return(list(plot1, plot2, plot3))
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
  write.csv(data, "summarised.csv", row.names = FALSE)
  latest_data <- data %>%
    group_by(country) %>%
    slice(c(n())) %>%
    ungroup() %>%
    filter(confirmed != 0)
  write.csv(latest_data, "latest.csv", row.names = FALSE)
  data %<>%
    add_rates()
  write.csv(data, "rates.csv", row.names = FALSE)
  ready_data <- ready_plot_data(data)
  plots <- list()
  plots[["top10"]] <- plot_top10_confirmed(ready_data)
  plots[["cases"]] <- plot_world_cases(data, ready_data)
  data_world <-
    data %>%
    filter(country == "World")
  data_india <-
    data %>%
    filter(country == "India") %>%
    filter(confirmed != 0)
  plots[["confirmed"]] <- plot_current_confirmed(data_world, "World")
  plots[["deaths"]] <- plot_deaths_recovered(data_world, "World")
  plots[["rates"]] <- plot_death_rates(data_world, "World")
  plots[["confirmed_india"]] <- plot_current_confirmed(data_india, "India")
  plots[["deaths_india"]] <- plot_deaths_recovered(data_india, "India")
  plots[["rates_india"]] <- plot_death_rates(data_india, "India")
  print(plots[["top10"]], newpage = FALSE)
  plots[["top10"]] <- NULL
  invisible(lapply(plots, print_chart))
  latest_to_pdf(latest_data, "latestreport.pdf")
  output_to_pdf(data_world, "worldreport.pdf")
  output_to_pdf(data_india, "indiareport.pdf")
  write.csv(data_world, "world_data.csv", row.names = FALSE)
  write.csv(data_india, "india_data.csv", row.names = FALSE)
  return(0)
}

print_chart <- function(plot) {
  if ("grob" %in% class(plot)) {
    grid::grid.newpage()
    grid::grid.draw(plot, recording = FALSE)
  } else {
    if ("list" %in% class(plot)) {
      lapply(plot, print)
    } else {
      print(plot)
    }
  }
}

plot_deaths_recovered <- function(data, title) {
  ## a scatter plot with a smoothed line and vertical x-axis labels
  plot1 <- ggplot(
    data =
      subset(
        data,
        !is.na(
          deaths
        )
      ),
    aes(x = date, y = deaths)
  ) +
    geom_point() +
    geom_smooth(
      method = "loess",
      formula = y ~ x
    ) +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Deaths")
  plot2 <- ggplot(
    data =
      subset(
        data,
        !is.na(
          recovered
        )
      ),
    aes(x = date, y = recovered)
  ) +
    geom_point() +
    geom_smooth(
      formula = y ~ x,
      method = "loess"
    ) +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Recovered Cases")
  plot3 <- ggplot(
    data =
      subset(
        data,
        !is.na(
          deaths.inc
        )
      ),
    aes(x = date, y = deaths.inc)
  ) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess") +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Increase in Deaths")
  plot4 <-
    ggplot(
      data =
        subset(
          data,
          !is.na(recovered.inc)
        ),
      aes(x = date, y = recovered.inc)
    ) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess") +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Increase in Recovered Cases")
  ## show four plots together, with 2 plots in each row
  grob <- gridExtra::arrangeGrob(plot1, plot2, plot3, plot4,
    nrow = 2,
    top = title
  )
  return(grob)
}

plot_death_rates <- function(data, title) {
  ## three death rates
  n <- nrow(data)
  print(n)
  plot1 <- ggplot(data, aes(x = date), na.rm = TRUE) +
    geom_line(aes(y = rate.upper, colour = "Upper bound")) +
    geom_line(aes(y = rate.lower, colour = "Lower bound")) +
    geom_line(aes(y = rate.daily, colour = "Daily")) +
    xlab("Date") +
    ylab("Death Rate (%)") +
    labs(title = "Overall") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ylim(0, 100)
  ## focusing on last 2 weeks
  plot2 <- ggplot(data[n - (14:0), ], aes(x = date), na.rm = TRUE) +
    geom_line(aes(y = rate.upper, colour = "Upper bound")) +
    geom_line(aes(y = rate.lower, colour = "Lower bound")) +
    geom_line(aes(y = rate.daily, colour = "Daily")) +
    xlab("Date") +
    ylab("Death Rate (%)") +
    labs(title = "Last two weeks") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ylim(0, 100)
  grob <- gridExtra::arrangeGrob(plot1, plot2,
    ncol = 2,
    top = title
  )
  return(grob)
}

plot_current_confirmed <- function(data, title) {
  ## current confirmed and its increase

  plot1 <- ggplot(
    data =
      subset(
        data,
        !is.na(data$remaining.confirmed)
      ),
    aes(
      x = date,
      y = remaining.confirmed
    )
  ) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess") +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Current Confirmed Cases")
  plot2 <- ggplot(
    data =
      subset(
        data,
        !is.na(data$confirmed.inc)
      ),
    aes(x = date, y = confirmed.inc)
  ) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess") +
    xlab("Date") +
    ylab("Count") +
    labs(title = "Increase in Current Confirmed")
  grob <- gridExtra::arrangeGrob(plot1, plot2,
    ncol = 2,
    top = title
  )
  return(grob)
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
          )),
      confirmed.rt =
        ifelse(date == day1,
          NA,
          (confirmed - lag(confirmed,
            n = 1
          )) / lag(confirmed, n = 1)
        ) %>% round(4)
    )
  ## death rate based on total deaths and cured cases
  data %<>%
    mutate(
      rate.upper =
        ifelse(is.nan(100 * deaths /
          (deaths + recovered)), 0,
        100 * deaths /
          (deaths + recovered)
        ) %>%
          round(1)
    )
  ## lower bound: death rate based on total confirmed cases
  data %<>%
    mutate(
      rate.lower =
        ifelse(is.nan(100 * deaths /
          confirmed), 0,
        100 * deaths /
          confirmed
        ) %>%
          round(1)
    )
  ## death rate based on the number of death/cured on every single day
  data %<>%
    mutate(
      rate.daily =
        ifelse((is.nan(100 * deaths.inc /
          (deaths.inc +
            recovered.inc)) |
          is.na(100 * deaths.inc /
            (deaths.inc +
              recovered.inc))), 0,
        100 * deaths.inc /
          (deaths.inc +
            recovered.inc)
        ) %>%
          round(1)
    )
  return(data)
}

ready_plot_data <- function(data) {
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
  ready_data <- list(
    max.date = max_date,
    data.latest = data_latest,
    top.countries = top_countries
  )
  return(ready_data)
}

plot_top10_confirmed <- function(ready_data) {
  max_date <- ready_data$max.date
  ## ranking by confirmed cases
  data_latest <- ready_data$data.latest
  ## top 10 countries: 12 incl. 'World' and 'Others'
  top_countries <- ready_data$top.countries

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

  plot <- df %>% ggplot(aes(fill = country)) +
    geom_bar(aes(x = "", y = per), stat = "identity") +
    coord_polar("y", start = 0) +
    xlab("") +
    ylab("Percentage (%)") +
    labs(title = paste0(
      "Top 10 Countries with Most Confirmed Cases (",
      max_date, ")"
    )) +
    scale_fill_discrete(name = "Country", labels = df$txt)
  return(plot)
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

latest_to_pdf <- function(data, filename) {
  date <- head(data, 1)$date
  data %<>% select(c(
    country, confirmed, deaths, recovered, remaining.confirmed
  ))
  ## output as a table
  data %>%
    kable("latex",
      booktabs = TRUE,
      longtable = TRUE,
      caption = paste(
        "Latest World Report",
        date
      ),
      format.args = list(big.mark = ",")
    ) %>%
    kable_styling(
      font_size = 5,
      full_width = TRUE,
      latex_options =
        c(
          "striped",
          "hold_position",
          "repeat_header"
        )
    ) %>%
    save_kable(filename,
      keep_tex = TRUE
    )
}

output_to_pdf <- function(data, filename) {
  ## re-order columns
  # deadIncr, curedIncr,
  data %<>% select(c(
    date, confirmed, deaths, recovered, remaining.confirmed, confirmed.rt,
    confirmed.inc, deaths.inc, recovered.inc, rate.upper, rate.daily, rate.lower
  ))
  ## to make column names shorter for output purpose only
  names(data) %<>%
    gsub(
      pattern = "Count",
      replacement = ""
    )
  ## output as a table
  data %>%
    kable("latex",
      booktabs = T, longtable = T, caption = paste("Cases in", filename),
      format.args = list(big.mark = ",")
    ) %>%
    kable_styling(
      font_size = 5,
      latex_options =
        c(
          "striped",
          "hold_position",
          "repeat_header"
        )
    ) %>%
    landscape() %>%
    save_kable(filename,
      keep_tex = TRUE
    )
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

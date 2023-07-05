#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : tsgraphics
# @created     : Monday Jul 03, 2023 14:44:40 IST
#
# @description : Time series graphics
######################################################################
suppressMessages(library(dplyr))
suppressMessages(library(tsibble))
suppressMessages(library(tsibbledata))
suppressMessages(library(readr))
suppressMessages(library(utils))
suppressMessages(library(ggplot2))
suppressMessages(library(feasts))

y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)
y
z <- tibble(
  Month = c('2019 Jan', '2019 Feb', '2019 Mar', '2019 Apr', '2019 May'),
  Observation = c(50, 23, 34, 30, 25),
)
z
z <- z |>
  dplyr::mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month)
z

options(dplyr.print_max = Inf)
options(pillar.width = Inf)
olympic_running
olympic_running |> distinct(Sex)
options(dplyr.print_max = NULL)
options(pillar.width = NULL)
PBS
PBS |>
  filter(ATC2 == "A10")
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost)
PBS |>
  filter(ATC2 == "A10") |>
  select(Cost)
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost))
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC/1e6)
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10
a10
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
      title = "Australian antidiabetic drug sales")

# Seasonal plot
a10 |>
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
      title = "Seasonal plot: Antidiabetic drug sales")

# Separate mini time plots
a10 |>
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

prisonfileurl <- "https://OTexts.com/fpp3/extrafiles/prison_population.csv"
prisonfile <- "prison_population.csv"

if (!file.exists(prisonfile)) {
utils::download.file(prisonfileurl, prisonfile)
}

prison <- readr::read_csv(prisonfile, show_col_types = FALSE)
spec(prison)
prison <- prison |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(key = c(State, Gender, Legal, Indigenous),index = Quarter)
prison

ansett

melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
      subtitle = "Melbourne-Sydney",
      y = "Passengers ('000)")

vic_elec
vic_elec |> gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")
vic_elec |> gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

vic_elec |> gg_season(Demand, period = "year") +
  labs(y="MWh", title="Electricity demand: Victoria")

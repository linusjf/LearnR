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

#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(tidyverse))
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
    ggplot(aes(x, f)) +
    geom_line()

#!/usr/bin/env Rscript
library(titanic)
data("titanic_train")
library(faraway)
data("worldcup")
library(ggplot2)

main <- function(argv) {
ggplot(data = titanic_train) +
       geom_histogram(aes(x = Fare), bins = 15)
ggplot2::ggsave("titanicfarehist.pdf")
ggplot(worldcup, aes(x = Time, y = Passes)) +
  geom_point()
ggplot2::ggsave("wctimepasses.pdf")
ggplot(worldcup, aes(x = Time, y = Passes,
                     color = Position, size = Shots)) +
  geom_point()
ggplot2::ggsave("wctimepassescolor.pdf")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

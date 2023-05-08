#!/usr/bin/env Rscript
# Comment: Inspired by Figure 3.3 from Wickham's book 'ggplot2'
library(ggplot2)

main <- function(argv) {
  print(ggplot(data = mpg, aes(x = displ, y = hwy, shape = factor(cyl))) +
    geom_point() +
    stat_smooth(method = "lm", colour = "black") +
    scale_shape_manual(values = c(
      1,
      16, 3, 17
    )) +
    theme_bw())
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

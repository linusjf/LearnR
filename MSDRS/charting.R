#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(faraway)
data("nepali")
library(ggplot2)
suppressMessages(library(GGally))

main <- function(argv) {
  print(head(faraway::nepali))
  nepali <- nepali %>%
    select(id, sex, wt, ht, age) %>%
    mutate(id = factor(id), sex = factor(sex, levels = c(1, 2), labels = c("Male", 
      "Female"))) %>%
    distinct(id, .keep_all = TRUE)
  print(head(nepali))
  ggplot(nepali, aes(x = ht)) + geom_histogram(fill = "lightblue", color = "black") + 
    ggtitle("Height of children") + xlab("Height (cm)") + xlim(c(0, 120))
  ggplot2::ggsave("nepalihts.pdf")
  ggplot(nepali, aes(x = ht, y = wt)) + geom_point(color = "blue", size = 0.5) + 
    ggtitle("Weight versus Height") + xlab("Height (cm)") + ylab("Weight (kg)")
  ggplot2::ggsave("nepalihtswts.pdf")
  ggplot(nepali, aes(x = ht, y = wt, color = sex)) + geom_point(size = 0.5) + ggtitle("Weight versus Height") + 
    xlab("Height (cm)") + ylab("Weight (kg)")
  ggplot2::ggsave("nepalihtswtssex.pdf")
  ggplot(nepali, aes(x = 1, y = ht)) + geom_boxplot() + xlab("") + ylab("Height (cm)")
  ggplot2::ggsave("nepaliboxplot.pdf")
  ggplot(nepali, aes(x = sex, y = ht)) + geom_boxplot() + xlab("Sex") + ylab("Height (cm)")
  ggplot2::ggsave("nepaliboxplotsex.pdf")
  pairs <- GGally::ggpairs(nepali %>%
    select(sex, wt, ht, age))
  ggplot2::ggsave(plot = pairs, filename = "ggpairs.pdf")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

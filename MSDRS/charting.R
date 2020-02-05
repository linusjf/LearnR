#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(faraway)
data("nepali")
library(ggplot2)

main <- function(argv) {
  print(head(faraway::nepali))
  nepali <- nepali %>%
  select(id, sex, wt, ht, age) %>%
  mutate(id = factor(id),
         sex = factor(sex, levels = c(1, 2),
                      labels = c("Male", "Female"))) %>%
  distinct(id, .keep_all = TRUE)
  print(head(nepali))
  ggplot(nepali, aes(x = ht)) +
  geom_histogram(fill = "lightblue",
                 color = "black") +
  ggtitle("Height of children") +
  xlab("Height (cm)") +
  xlim(c(0, 120))
  ggplot2::ggsave("nepalihts.pdf")
  ggplot(nepali, aes(x = ht, y = wt)) +
  geom_point(color = "blue", size = 0.5) +
  ggtitle("Weight versus Height") +
  xlab("Height (cm)") + ylab("Weight (kg)")
  ggplot2::ggsave("nepalihtswts.pdf")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

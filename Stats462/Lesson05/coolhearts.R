#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(ggplot2))


main <- function(argv) {
  data <- read.table("../Data/coolhearts.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Infarc ~ Area + X2 + X3, data = data)
  print(reg)

# save predictions of the model in the new data frame
# together with variable you want to plot against
predicted_df <- data.frame(Infarc = predict(reg, data),
                          Area = data$Area, Group = data$Group)

data %>% ggplot(aes(x = Area, y = Infarc, color = factor(Group))) +
  geom_point()
ggplot2::ggsave("coolhearts.pdf")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

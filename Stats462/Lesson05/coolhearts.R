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
labels <- c("Early cooling",
"Late cooling",
"No cooling")

data %>%
  ggplot(aes(x = Area, y = Infarc, color = factor(Group, labels = labels))) +
  ggtitle("Scatter plot of infarction versus area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Size of area at risk (grams)") +
  ylab("Size of Infarcted area (grams)") +
  labs(color = "Group") +
  geom_point(size = 3) +
  geom_line(color = "red", data = subset(predicted_df, Group == 1)) +
  geom_line(color = "green", data = subset(predicted_df, Group == 2)) +
  geom_line(color = "blue", data = subset(predicted_df, Group == 3))
ggplot2::ggsave("coolhearts.pdf")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

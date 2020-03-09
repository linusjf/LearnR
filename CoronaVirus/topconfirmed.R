#!/usr/bin/env Rscipt

library(coronavirus)
library(dplyr)

main <- function(argv) {
## get the COVID-19 data
data(coronavirus::coronavirus)
# Get top confirmed cases by state
data <- coronavirus %>%
filter(type == "confirmed") %>%
group_by(Country.Region) %>%
summarise(total = sum(cases)) %>%
arrange(-total) %>%
head(20)
print(data)
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

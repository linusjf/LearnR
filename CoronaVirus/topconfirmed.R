#!/usr/bin/env Rscript

library(coronavirus)
## get the COVID-19 data
data(coronavirus)
suppressMessages(library(dplyr))

main <- function(argv) {
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

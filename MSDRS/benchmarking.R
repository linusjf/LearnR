#!/usr/bin/env Rscript
library(microbenchmark)
library(ggplot2)
suppressMessages(library(dplyr))
suppressMessages(library(dlnm))
data("chicagoNMMAPS")

# Function that uses a loop
find_records_1 <- function(datafr, threshold) {
  highest_temp <- c()
  record_temp <- c()
  for (i in seq_len(nrow(datafr))) {
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold &
      datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
  return(datafr)
}

# Function that uses tidyverse functions
find_records_2 <- function(datafr, threshold) {
  datafr <- datafr %>%
    mutate(over_threshold = (temp >= threshold),
            cummax_temp = (temp == cummax(temp)),
            record_temp = (over_threshold & cummax_temp)) %>%
    select("date", "temp", "record_temp")
  return(as.data.frame(datafr))
}

main <- function(argv) {
print(microbenchmark(a <- rnorm(1000),
               b <- mean(rnorm(1000))))
example_data <- tibble(date = c("2015-07-01", "2015-07-02",
                                    "2015-07-03", "2015-07-04",
                                    "2015-07-05", "2015-07-06",
                                    "2015-07-07", "2015-07-08"),
                           temp = c(26.5, 27.2, 28.0, 26.9,
                                    27.5, 25.9, 28.0, 28.2))

test_1 <- find_records_1(example_data, 27)
print(test_1)
test_2 <- find_records_2(example_data, 27)
print(test_2)
print(all.equal(test_1, test_2))
record_temp_perf <- microbenchmark(find_records_1(example_data, 27),
                                   find_records_2(example_data, 27))
print(record_temp_perf)
record_temp_perf_2 <- microbenchmark(find_records_1(chicagoNMMAPS, 27),
                                     find_records_2(chicagoNMMAPS, 27))
print(record_temp_perf_2)
autoplot(record_temp_perf)
ggplot2::ggsave("firstperftest.pdf")
autoplot(record_temp_perf_2)
ggplot2::ggsave("secondperftest.pdf")
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

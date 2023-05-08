#!/usr/bin/env Rscript
# define x as male heights from dslabs data
suppressPackageStartupMessages(library(tidyverse))
library(dslabs)
data(heights)
x <- heights %>%
  filter(sex == "Male") %>%
  pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color = "black", binwidth = 2)

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s) # generate 800 normally distributed random heights
  # determine the tallest height
  max(simulated_data)
})
print("proportion of 7 footers")
mean(tallest >= 7 * 12)

x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

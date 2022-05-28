#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(tidyverse))
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

F <- function(a) mean(x <= a)

# probability of male taller than 70
1 - F(70)    

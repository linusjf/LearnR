#!/usr/bin/env Rscript
library(gtools)
# ways to choose 2 numbers in order from 1:5
permutations(5,2)    
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

# order matters
permutations(3,2)    
# order does not matter
combinations(3,2)    

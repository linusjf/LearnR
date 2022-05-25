#!/usr/bin/env Rscript
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
# check if any birthdays are duplicated
any(duplicated(bdays))   

# Monte Carlo simulation with B=10000 replicates
B <- 10000
# returns vector of B logical values
results <- replicate(B, {    
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
})
# calculates
mean(results)    

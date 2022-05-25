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

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
	same_day <- replicate(B, {
    	bdays <- sample(1:365, n, replace = TRUE)
        any(duplicated(bdays))
    })
    mean(same_day)
}

# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
    prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
    # calculate prob of no shared birthdays and subtract from 1
    1 - prod(prob_unique)    
}

n <- seq(1, 60)
prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
# plot Monte Carlo results
plot(n, prob)    
lines(n, eprob, col = "red")

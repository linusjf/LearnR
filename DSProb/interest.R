#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(tidyverse))

set.seed(1)
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
    defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
    sum(defaults * loss_per_foreclosure)
})

data.frame(losses_in_millions = losses/10^6) %>%
    ggplot(aes(losses_in_millions)) +
    geom_histogram(binwidth = 0.6, col = "black")

# expected value 
n * (p * loss_per_foreclosure + (1-p) * 0)   
# standard error
sqrt(n) * abs(loss_per_foreclosure) * sqrt(p * (1-p)) 

# Calculating interest rates for expected value of 0
# We can calculate the amount  to add to each loan so that the expected value is 0 
# using the equation. 
# Note that this equation is the definition of expected value given a loss per
# foreclosure l  
# with foreclosure probability p  and profit x if there is no foreclosure
# (probability).
# We solve for -l * p / (1-p) and calculate x:
x = - loss_per_foreclosure * p/(1-p)
x
print("On a $180,000 loan, this equals an interest rate of:")
x/180000

# Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
print("interest rate:")
x/180000    
print("expected value of the profit per loan")
loss_per_foreclosure * p + x * (1-p)    
print("expected value of the profit over n loans")
n * (loss_per_foreclosure * p + x * (1-p))

B <- 100000
profit <- replicate(B, {
    draws <- sample( c(x, loss_per_foreclosure), n, 
                        prob=c(1-p, p), replace = TRUE) 
    sum(draws)
})
print("expected value of the profit over n loans")
mean(profit)    
print("probability of loss from loans to bank:")
mean(profit<0)   

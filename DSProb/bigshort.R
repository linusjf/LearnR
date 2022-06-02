#!/usr/bin/env Rscript
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
print("Interest on loan:")
x <- r * 180000
x
print("Expected value per loan:")
loss_per_foreclosure * p + x * (1-p)

print("Calculating number of loans for desired probability of losing money")
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
print("number of loans required")
n    
print("expected profit over n loans")
n*(loss_per_foreclosure*p + x * (1-p))   

print("Monte Carlo simulation with known default probability")
print("This Monte Carlo simulation estimates the expected profit given a known probability of default 0.4.")

set.seed(100)
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
    draws <- sample(c(x, loss_per_foreclosure), n, prob=c(1-p, p), replace = TRUE)
    sum(draws)
})
mean(profit)

print("Monte Carlo simulation with unknown default probability")
print("This Monte Carlo simulation estimates the expected profit given an unknown probability of default , modeling the situation where an event changes the probability of default for all borrowers simultaneously.")

p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
    new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
    draws <- sample( c(x, loss_per_foreclosure), n, 
                        prob=c(1-new_p, new_p), replace = TRUE)
    sum(draws)
})
print("expected profit")
mean(profit)    
print("probability of losing money")
mean(profit < 0)    
print("probability of losing more than 10 million")
mean(profit < -10000000)    

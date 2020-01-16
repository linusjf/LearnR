#!/usr/bin/env Rscript
library(compiler)
library(rbenchmark)
my_mean = function(x) {
    total = 0
    n = length(x)
    for (i in 1:n) total = total + x[i]/n
    total
}
cmpmean <- cmpfun(my_mean)
## Generate some data
x = rnorm(10000)
benchmark(my_mean(x), cmpmean(x), mean(x), columns = c("test", "elapsed", "relative"), 
    order = "relative", replications = 5000)

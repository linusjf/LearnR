#!/usr/bin/env Rscript
# 1: Create large dataset
X = data.frame(matrix(rnorm(1e+07), nrow = 1e+06))

startTime <- proc.time()

# 2: Find the median of each column using a single core
r1 = lapply(X, median)
endTime <- proc.time() - startTime
print(endTime)
startTime2 <- proc.time()

# 3: Find the median of each column using many cores XXX: Change to function from
# package
r2 = parallel::mclapply(X, median)
# runs in serial on Windows
endTime2 <- proc.time() - startTime2
print(endTime2)

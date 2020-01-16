#!/usr/bin/env Rscript
# 1: Create large dataset
x <- data.frame(matrix(rnorm(1e+07), nrow = 1e+06))

start_time <- proc.time()

# 2: Find the median of each column using a single core
r1 <- lapply(x, median)
end_time <- proc.time() - start_time
print(end_time)
start_time2 <- proc.time()

# 3: Find the median of each column
# using many cores
# xxx: Change to function from package
r2 <- parallel::mclapply(x, median)
# runs in serial on Windows
end_time2 <- proc.time() - start_time2
print(end_time2)

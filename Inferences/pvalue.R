#!/usr/bin/env Rscript
require(ggplot2)
require(tidyr)
friends <- paste0("friend_", 1:10)
friends_daniel <- c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9)
friends_kyra <- c(9, 6, 7, 8, 7, 9, 8, 8, 8, 7)
df <- data.frame(friends, friends_daniel, friends_kyra)
colnames(df) <- c("", "Daniel", "Kyra")
print(df)
df_long <- tidyr::gather(df, "Friend Group", rating, "Daniel":"Kyra", factor_key = TRUE)
print(df_long)

backgroundcolor <- "#fffafa"
# Plot the data
ggplot(df_long, aes(x = rating, fill = `Friend Group`)) +
  geom_density(alpha = .3) +
  theme_bw() +
  theme(plot.background = element_rect(fill = backgroundcolor))  +
  theme(panel.background = element_rect(fill = backgroundcolor))


tstat <- t.test(df_long$rating ~ df_long$`Friend Group`, var.equal = TRUE)
se <- tstat$stderr

par(bg = backgroundcolor)
x <- seq(-5, 5, length = 100)
plot(x, dt(x, df = 18), col = "black", type = "l", xlab = "t-value", ylab = "Density", main = "t-distribution", lwd = 2)
x <- seq(2.5175, 5, length = 100)
z <- (dt(x, df = 18))
polygon(c(2.5175, x, 8), c(0, z, 0), col = rgb(1, 0, 0, 0.5))
x <- seq(-5, -2.5175, length = 100)
z <- (dt(x, df = 18))
polygon(c(-8, x, -2.5175), c(0, z, 0), col = rgb(1, 0, 0, 0.5))

# plot histogram for friends data
p <- numeric(100000) # store all simulated *p*-values
for (i in 1:100000) { # for each simulated experiment
  x <- rnorm(n = 10, mean = 7.7, sd = se) # Simulate data
  y <- rnorm(n = 10, mean = 8.7, sd = se) # Simulate data
  p[i] <- t.test(x, y)$p.value # store the *p*-value
}
power <- sum(p < 0.05) / 100000 # compute power
print(power)
hist(p, breaks = 20) # plot a histogram


#Set number of simulations
nSims <- 100000 # number of simulated experiments
p <- numeric(nSims) # set up empty variable to store all simulated *p*-values
bars <- 20

for (i in 1:nSims) { # for each simulated experiment
  x <- rnorm(n = 71, mean = 100, sd = 15) # Simulate data
  y <- rnorm(n = 71, mean = 105, sd = 15) # Simulate data
  p[i] <- t.test(x, y)$p.value # store the *p*-value
}

#Plot figure
par(bg = backgroundcolor)
op <- par(mar = c(5,7,4,4)) #change white-space around graph
hist(p, breaks=bars, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
    main=paste("P-value Distribution with 50% power"),
    col="grey", xlim=c(0,1), ylim=c(0, nSims))
axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
axis(side=2, at=seq(0,nSims, nSims/4), labels=seq(0,nSims, nSims/4), las=2)
abline(h=nSims/bars, col = "red", lty=3)
power <- sum(p < 0.05) / 100000 # compute power
print(power)

n <- 71 # sample size per independent group
d <- 0.33 # effect size
se <- sqrt(2 / n) # standard error
ncp <- (d * sqrt(n / 2)) # non-centrality parameter d

pdf2_t <- function(p) { #probability density function
  0.5 * dt(qt(p / 2, 2 * n - 2, 0), 2 * n - 2, ncp) /
      dt(qt(p / 2, 2 * n - 2, 0), 2 * n - 2, 0) +
    dt(qt(1 - p / 2, 2 * n - 2, 0), 2 * n - 2, ncp) /
      dt(qt(1 - p / 2, 2 * n - 2, 0), 2 * n - 2, 0)
}

par(bg = backgroundcolor)
plot(-10, xlab = "P-value", ylab = "Density", axes = FALSE,
    main = "P-value distribution", xlim = c(0, 1), ylim = c(0, 50),
    cex.lab = 1.5, cex.main = 1.5, cex.sub = 1
)
curve(pdf2_t, 0, 1, n = 1000, col = "black", lty = 1, lwd = 3, add = TRUE)
axis(side = 1, at = seq(0, 1, 0.05),
    labels = formatC(seq(0, 1, 0.05), format = "f", digits = 2), cex.axis = 1)

#| fig-cap: "Distribution of *p*-values when the null hypothesis is true."

#Set number of simulations
nSims <- 100000 #number of simulated experiments
p <-numeric(nSims) #set up empty variable to store all simulated *p*-values

for (i in 1:nSims) { # for each simulated experiment
  x <- rnorm(n = 71, mean = 100, sd = 15) # Simulate data
  y <- rnorm(n = 71, mean = 100, sd = 15) # Simulate data
  p[i] <- t.test(x, y)$p.value # store the *p*-value
}

bars<-20
#Plot figure
op <- par(mar = c(5,7,4,4)) #change white-space around graph
par(bg = backgroundcolor)
hist(p, breaks=bars, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
    main=paste("P-value distribution when the null hypothesis is true"),
    col="grey", xlim=c(0,1), ylim=c(0, nSims))
axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
axis(side=2, at=seq(0,nSims, nSims/4), labels=seq(0,nSims, nSims/4), las=2)
abline(h=nSims/bars, col = "red", lty=3)

#| fig-cap: "*P*-value distribution for 0 (grey horizontal line, 50 percent power (black solid curve), and 99 percent power (black dotted curve, where *p*-values just below 0.05 are more likely when $H_0$ is true than when $H_1$ is true)."
# Lindley plot

n <- 150
p <- 0.05
ymax <- 25 # Maximum value y-scale (only for p-curve)

# Calculations

# p-value function
pdf2_t <- function(p) 0.5 * dt(qt(p / 2, 2 * n - 2, 0), 2 * n - 2, ncp) / dt(qt(p / 2, 2 * n - 2, 0), 2 * n - 2, 0) + dt(qt(1 - p / 2, 2 * n - 2, 0), 2 * n - 2, ncp) / dt(qt(1 - p / 2, 2 * n - 2, 0), 2 * n - 2, 0)

par(bg = backgroundcolor)
plot(-10,
  xlab = "P-value", ylab = "Density", axes = FALSE,
  main = "P-value distribution for d = 0, 50% power, and 99% power", xlim = c(0, 1), ylim = c(0, ymax), cex.lab = 1.2, cex.main = 1.2, cex.sub = 1
)
axis(side = 1, at = seq(0, 1, 0.05), labels = formatC(seq(0, 1, 0.05), format = "f", digits = 2), cex.axis = 1)
# Draw null line
ncp <- (0 * sqrt(n / 2)) # Calculate non-centrality parameter d
curve(pdf2_t, 0, 1, n = 1000, col = "grey", lty = 1, lwd = 2, add = TRUE)
# Draw 50% low power line
n <- 146
d <- 0.23
se <- sqrt(2 / n) # standard error
ncp <- (d * sqrt(n / 2)) # Calculate non-centrality parameter d
curve(pdf2_t, 0, 1, n = 1000, col = "black", lwd = 3, add = TRUE)
# Draw 99% power line
n <- 150
d <- 0.5
se <- sqrt(2 / n) # standard error
ncp <- (d * sqrt(n / 2)) # Calculate non-centrality parameter d
curve(pdf2_t, 0, 1, n = 1000, col = "black", lwd = 3, lty = 3, add = TRUE)

add_type1_error <- function(N,
                            side = "right",
                            col = rgb(1, 0, 0, 0.5)) {
  mult <- ifelse(side == "right", 1, -1)
  crit_d <- mult * abs(qt(0.05 / 2, (N * 2) - 2)) / sqrt(N / 2)

  if (side == "right") {
    y <- seq(crit_d, 10, length = 10000)
  } else {
    y <- seq(-10, crit_d, length = 10000)
  }

  # determine upperbounds polygon
  suppressWarnings({
    z <- (dt(y * sqrt(N / 2), df = (N * 2) - 2) * sqrt(N / 2))
  })

  if (side == "right") {
    polygon(c(crit_d, y, 10), c(0, z, 0), col = col)
  } else {
    polygon(c(y, crit_d, crit_d), c(z, 0, 0), col = col)
  }
}

# calculate distribution of d based on t-distribution
calc_d_dist <- function(x, N, ncp = 0) {
  suppressWarnings({
    # generates a lot of warnings sometimes
    dt(x * sqrt(N / 2), df = (N * 2) - 2, ncp = ncp) * sqrt(N / 2)
  })
}

#| fig-cap: "Distribution of observed Cohen's *d* effect sizes when collecting 50 observations per group in an independent *t*-test."# Figure 1 & 2 (set to N <- 5000 for Figure 2)
# Set x-axis upper and lower scale points
low_x <- -1
high_x <- 1
y_max <- 2

# Set sample size per group and effect size d (assumes equal sample sizes per group)
N <- 50 # sample size per group for independent t-test
d <- 0.5 # please enter positive d only
# Calculate non-centrality parameter - equals t-value from sample
ncp <- d * sqrt(N / 2)

# # or Cumming, page 305
# ncp <- d / (sqrt((1 / N) + (1 / N)))

# calc d-distribution
x <- seq(low_x, high_x, length = 10000) # create x values
d_dist <- calc_d_dist(x, N, ncp)

# Set max Y
y_max <- max(d_dist) + 0.5

# create plot
par(bg = backgroundcolor)
plot(-10, xlim = c(low_x, high_x), ylim = c(0, y_max), xlab = "Difference", ylab = "", main = paste("null hypothesis for N = ", N))

d_dist <- dt(x * sqrt(N / 2), df = (N * 2) - 2, ncp = 0) * sqrt(N / 2)
lines(x, d_dist, col = "black", type = "l", lwd = 2)

# Add type 1 error rate
add_type1_error(N, "right")
add_type1_error(N, "left")

#| fig-cap: "Distribution of observed Cohen's *d* effect sizes when collecting 5000 observations per group in an independent *t*-test when *d* = 0."

low_x <- -1
high_x <- 1
y_max <- 2

# Set sample size per group and effect size d (assumes equal sample sizes per group)
N <- 5000 # sample size per group for independent t-test
d <- 0.5 # please enter positive d only
# Calculate non-centrality parameter - equals t-value from sample
ncp <- d * sqrt(N / 2)

# calc d-distribution
x <- seq(low_x, high_x, length = 10000) # create x values
d_dist <- calc_d_dist(x, N, ncp)

# Set max Y
y_max <- max(d_dist) + 0.5

# create plot
par(bg = backgroundcolor)
plot(-10, xlim = c(low_x, high_x), ylim = c(0, y_max), xlab = "Difference", ylab = "", main = paste("null hypothesis for N = ", N))

d_dist <- calc_d_dist(x, N, 0)
lines(x, d_dist, col = "black", type = "l", lwd = 2)

# Add type 1 error rate
add_type1_error(N, "right")
add_type1_error(N, "left")

low_x <- -1
high_x <- 1.5
y_max <- 2

# Set sample size per group and effect size d (assumes equal sample sizes per group)
N <- 50 # sample size per group for independent t-test
d <- 1.5 # please enter positive d only
# Calculate non-centrality parameter - equals t-value from sample
ncp <- d * sqrt(N / 2)

# # or Cumming, page 305
# ncp <- d / (sqrt((1 / N) + (1 / N)))

# calc d-distribution
x <- seq(low_x, high_x, length = 10000) # create x values
d_dist <- calc_d_dist(x, N, ncp)

# Set max Y
y_max <- max(d_dist) + 0.5

par(bg = backgroundcolor)
plot(-10, xlim = c(low_x, high_x), ylim = c(0, y_max), xlab = "Difference", ylab = "", main = paste("Null and alternative hypothesis for N = ", N))
lines(x, d_dist, col = "black", type = "l", lwd = 2)
# add d = 0 line
d_dist <- calc_d_dist(x, N, ncp=0)
lines(x, d_dist, col = "darkgrey", type = "l", lwd = 2)

# Add type 1 error rate
add_type1_error(N, "right")
add_type1_error(N, "left")

nsims <- 100000 # number of simulations

m <- 106 # mean sample
n <- 26 # set sample size
sd <- 15 # SD of the simulated data

p <- numeric(nsims) # set up empty vector
bars <- 20

for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.05) / nsims), 2) # power

# Plot figure
hist(p,
  breaks = bars, xlab = "P-values", ylab = "number of p-values\n",
  axes = FALSE, main = paste("P-value Distribution with",
                            round(power * 100, digits = 1), "% Power"),
  col = "grey", xlim = c(0, 1), ylim = c(0, nsims))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4),
    labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)

nsims <- 100000 # number of simulations

m <- 106 # mean sample
n <- 51 # set sample size
sd <- 15 # SD of the simulated data

p <- numeric(nsims) # set up empty vector
bars <- 20

for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.05) / nsims), 2) # power

# Plot figure
hist(p,
  breaks = bars, xlab = "P-values", ylab = "number of p-values\n",
  axes = FALSE, main = paste("P-value Distribution with",
                            round(power * 100, digits = 1), "% Power"),
  col = "grey", xlim = c(0, 1), ylim = c(0, nsims))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4),
    labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)

nsims <- 100000 # number of simulations

m <- 100 # mean sample
n <- 51 # set sample size
sd <- 15 # SD of the simulated data

p <- numeric(nsims) # set up empty vector
bars <- 20

for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.05) / nsims), 2) # power

# Plot figure
hist(p,
  breaks = bars, xlab = "P-values", ylab = "number of p-values\n",
  axes = FALSE, main = paste("P-value Distribution with",
                            round(power * 100, digits = 1), "% Power"),
  col = "grey", xlim = c(0, 1), ylim = c(0, nsims))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4),
    labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)

bars <- 100

for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.05) / nsims), 2) # power

# Plot figure
hist(p,
  breaks = bars, xlab = "P-values", ylab = "number of p-values\n",
  axes = FALSE, main = paste("P-value Distribution with",
                            round(power * 100, digits = 1), "% Power"),
  col = "grey", xlim = c(0, 0.05), ylim = c(0, nsims))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4),
    labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)

m <- 107 # mean sample
for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.05) / nsims), 2) # power

# Plot figure
hist(p,
  breaks = bars, xlab = "P-values", ylab = "number of p-values\n",
  axes = FALSE, main = paste("P-value Distribution with",
                            round(power * 100, digits = 1), "% Power"),
  col = "grey", xlim = c(0, 0.05), ylim = c(0, nsims))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4),
    labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)

for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.01) / nsims), 2) # power

# Plot figure
hist(p,
  breaks = bars, xlab = "P-values", ylab = "number of p-values\n",
  axes = FALSE, main = paste("P-value Distribution with",
                            round(power * 100, digits = 1), "% Power"),
  col = "grey", xlim = c(0, 0.05), ylim = c(0, nsims))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4),
    labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)

for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.01) / nsims), 2) # power

# Plot figure
hist(p,
  breaks = bars, xlab = "P-values", ylab = "number of p-values\n",
  axes = FALSE, main = paste("P-value Distribution with",
                            round(power * 100, digits = 1), "% Power"),
  col = "grey", xlim = c(0, 0.05), ylim = c(0, 10000))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4),
    labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)

n <- 51 # set sample size
m <- 108 # mean sample
for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.01) / nsims), 2) # power

# Plot figure
hist(p,
  breaks = bars, xlab = "P-values", ylab = "number of p-values\n",
  axes = FALSE, main = paste("P-value Distribution with",
                            round(power * 100, digits = 1), "% Power"),
  col = "grey", xlim = c(0, 0.05), ylim = c(0, 10000))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4),
    labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)

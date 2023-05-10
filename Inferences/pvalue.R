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

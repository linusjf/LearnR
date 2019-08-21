#!/usr/bin/env Rscript
aba <- read.csv("abalone.data", header = T, as.is = T)
grps <- list()
for (gen in c("M", "F")) grps[[gen]] <- which(aba == gen)
abam <- aba[grps$M, ]
abaf <- aba[grps$F, ]
pdf("abalone.pdf")  #set graphical output file 
plot(abam$Length, abam$Diameter, xlab = "Length", ylab = "Diameter", xlim = c(0, 
    0.8), ylim = c(0, 0.7))
par(new = TRUE)
plot(abaf$Length, abaf$Diameter, pch = "x", xlab = "Length", ylab = "Diameter", xlim = c(0, 
    0.8), ylim = c(0, 0.7))
dev.off()

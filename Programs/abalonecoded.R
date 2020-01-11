#!/usr/bin/env Rscript
aba <- read.csv("abalone.data", header = T, as.is = T)
pdf("abalonecoded.pdf")  #set graphical output file 
pchvec <- ifelse(aba$Sex == "M", "o", ifelse(aba$Sex == "F", "x", "."))
plot(aba$Length, aba$Diameter, xlab = "Length", ylab = "Diameter", xlim = c(0, 0.8), 
    ylim = c(0, 0.7), pch = pchvec)
graphics.off()

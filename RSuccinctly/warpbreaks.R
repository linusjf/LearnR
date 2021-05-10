#!/usr/bin/env Rscript
library(stats) 
library(graphics)
summary(warpbreaks)                    
opar <- par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))     
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "A", main = "Wool A")    
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "B", main = "Wool B")
mtext("warpbreaks data", side = 3, outer = TRUE)                              
par(opar)
summary(fm1 <- lm(breaks ~ wool*tension, data = warpbreaks))
anova(fm1)       

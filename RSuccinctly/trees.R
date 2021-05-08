#!/usr/bin/env Rscript
library(stats)
library(graphics)
pairs(trees, panel = panel.smooth, main = "trees data") 
plot(Volume ~ Girth, data = trees, log = "xy")     
coplot(log(Volume) ~ log(Girth) | Height, data = trees,
       panel = panel.smooth)
summary(fm1 <- lm(log(Volume) ~ log(Girth), data = trees))          
summary(fm2 <- update(fm1, ~ . + log(Height), data = trees))       
step(fm2)
## i.e., Volume ~= c * Height * Girth^2  seems reasonable

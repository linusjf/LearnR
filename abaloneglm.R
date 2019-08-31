
aba <- read.csv("abalone.data", header = T)
abamf <- aba[ aba$Sex != "I",] 
# exclude infants from the analysis 
lftn <- function( clmn) { 
  glm( abamf$Sex ~ clmn, family = binomial)$coef 
  } 
loall <- sapply(abamf[,-1],lftn)
library(stargazer)
stargazer(loall, type = "text")

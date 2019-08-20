#!/usr/bin/env Rscript
aba <-read.csv("abalone.data", header = T, as.is = T) 
grps <-list() 
for (gen in c("M", "F")) 
grps [[gen]] <- which(aba == gen)
abam <- aba[grps$M,] 
abaf <- aba[grps$F,] 
pdf("abalonecoded.pdf")  #set graphical output file 
pchvec <- ifelse(aba$Sex == "M","o",
                 ifelse(aba$Sex=="F","x","."))
print(pchvec)
plot( aba$Length, aba$Diameter,xlab="Length",ylab="Diameter",xlim=c(0,0.8),ylim=c(0,0.7),pch=pchvec)
#par(new=TRUE) 
#plot( abaf$Length, abaf$Diameter, pch ="x",xlab="Length",ylab="Diameter",xlim=c(0,0.8),ylim=c(0,0.7))
dev.off()

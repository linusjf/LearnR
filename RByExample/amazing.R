#!/usr/bin/env Rscript
library(stringi)

# Goal: The amazing R vector notation.

main = function(argv) {
  cat("EXAMPLE 1: sin(x) for a vector --\n")
  # Suppose you have a vector x --
  x = c(0.1,0.6,1.0,1.5)

  # The bad way --
  n = length(x)
  r = numeric(n)
  for (i in 1:n) 
    r[i] = sin(x[i])
  print(r)

  # The good way -- don't use loops --
  print(sin(x))


  cat("\n\nEXAMPLE 2: Compute the mean of every row of a matrix --\n")
  # Here's another example. It isn't really about R; it's about thinking in
  # matrix notation. But still.
  # Let me setup a matrix --
  N=4; M=100;
  r = matrix(runif(N*M), N, M)

  # So I face a NxM matrix
  #               [r11 r12 ... r1N]
  #               [r21 r22 ... r2N]
  #               [r32 r32 ... r3N]
  # My goal: each column needs to be reduced to a mean.

  # Method 1 uses loops:
  mean1 = numeric(M)
  for (i in 1:M) 
    mean1[i] = mean(r[,i])

  # Alternatively, just say:
  print(rep(1/N,N))
  mean2 = rep(1/N, N) %*% r               
  # Pretty!

  # The two answers are the same --
  print(all.equal(mean1,mean2[,]))
  print(all.equal(mean1,rowMeans(t(r))))
  #
  # As an aside, I should say that you can do this directly by using
  # the rowMeans() function. But the above is more about pedagogy rather
  # than showing you how to get rowmeans.

  print("Displaying row sums")
  print(rowSums(r))
  
  print("Displaying row means")
  print(rowMeans(r))

  # You can display col sums and col means as well.
  # Too many columns, so not displaying it here.

  cat("\n\nEXAMPLE 3: Nelson-Siegel yield curve\n")
  # Write this as if you're dealing with scalars --
  # Nelson Siegel function
  nsz <- function(b0, b1, b2, tau, t) {
    tmp = t/tau
    tmp2 = exp(-tmp)
    #return(b0 + ((b1+b2)*(1-tmp2)/(tmp)) - (b2*tmp2))
    return(beta0(b0,b1,b2,tau,t) + 
           beta1(b0,b1,b2,tau,t)
         + beta2(b0,b1,b2,tau,t)
         )
  }
  
  beta0 <- function(b0, b1, b2, tau, t) {
    return(b0)
  }
  
  beta1 <- function(b0, b1, b2, tau, t) {
    tmp = t/tau
    tmp2 = exp(-tmp)
    return(b1*(1-tmp2)/(tmp))
  }
  
  beta2 <- function(b0, b1, b2, tau, t) {
    tmp = t/tau
    tmp2 = exp(-tmp)
    return((b2*(1-tmp2)/tmp) - (b2*tmp2))
  }

  timepoints <- c(0.01,1:5)

  # The bad way:
  z <- numeric(length(timepoints))
  for (i in 1:length(timepoints)) 
    z[i] <- nsz(14.084,-3.4107,0.0015,1.8832,timepoints[i])
  print(z)

  # The R way --
  print(z <- nsz(14.084,-3.4107,0.0015,1.8832,timepoints))


  cat("\n\nEXAMPLE 3: Making the NPV of a bond--\n")
  # You know the bad way - sum over all cashflows, NPVing each.
  # Now look at the R way.
  C = rep(100, 6)
  # Print interest rates
  intrates = nsz(14.084,-3.4107,0.0015,1.8832,timepoints)       
  beta0s = rep(14.084,6) 
  print(beta0s)
  beta1s = beta1(14.084,-3.4107,0.0015,1.8832,timepoints)       
  print(beta1s)
  beta2s = beta2(14.084,-3.4107,0.0015,1.8832,timepoints)       
  print(beta2s)
  print("Interest rates...")
  print(intrates)
  # Print cashflows discounted @ 5%
  discounted = C/((1.05)^timepoints)
  print("Discounted cfs at 5%...")
  print(discounted)
  # Using NS instead of 5%
  cfs <- C/((1 + (0.01*nsz(14.084,-3.4107,0.0015,1.8832,timepoints))^timepoints)) 
  print("Cashflows at Nelson-Siegel rates...")
  print(cfs)

  print("Net Present Value...")
  # NPV in two different ways --
  npv1 <- C %*% (1 + (0.01*nsz(14.084,-3.4107,0.0015,1.8832,timepoints)))^-timepoints
  print(npv1)
  npv2 <- sum(C * (1 + (0.01*nsz(14.084,-3.4107,0.0015,1.8832,timepoints)))^-timepoints)
  print(npv2)
  print(all.equal(npv1[1,1],npv2))
  # You can drop back to a flat yield curve at 5% easily --
  npvflatsum <- sum(C * 1.05^-timepoints)
  print("Net Present Value at flat 5% rate...")
  print(npvflatsum)

  # Make a function for NPV --
  npv <- function(C, timepoints, r) {
    return(sum(C * (1 + (0.01*r))^-timepoints))
  }
  npv(C, timepoints, 5)

  # Bottom line: Here's how you make the NPV of a bond with cashflows C
  # at timepoints timepoints when the zero curve is a Nelson-Siegel curve --
  npv(C, timepoints, nsz(14.084,-3.4107,0.0015,1.8832,timepoints))
  
  # Wow!
  cat("\n\nPlotting graphs to amazing.pdf--\n")
  beta <- "\\U03B2"
  beta <- stri_unescape_unicode(gsub("\\U","\\u",beta, fixed=TRUE))
  pdf("amazing.pdf",onefile=TRUE)
  plot(timepoints,intrates,xlab="Time",ylab="Interest rates (N/S)")
  lines(timepoints,intrates)
  plot(timepoints,beta0s,xlab="Time",
       ylab=expression(paste(beta,"0 component (N/S)"))) 
  lines(timepoints,beta0s)
  plot(timepoints,beta1s,xlab="Time",
       ylab=expression(paste(beta,"1 component (N/S)")))
  lines(timepoints,beta1s)
  plot(timepoints,beta2s,xlab="Time",
       ylab=expression(paste(beta,"2 component (N/S)")))  
  lines(timepoints,beta2s)
  plot(timepoints,discounted,xlab="Time",ylab="Cash flows discounted at 5%")
  lines(timepoints,discounted)
  plot(timepoints,cfs,xlab="Time",ylab="Cash flows")
  lines(timepoints,cfs)
  graphics.off()

  cat("\n\nEXAMPLE 4: Testing performance--\n")
  # ---------------------------------------------------------------------------
  # Elegant vector notation is amazingly fast (in addition to being beautiful)
  N <- 1e5
  x <- runif(N, -3,3)
  y <- runif(N)

  method1 <- function(x,y) {
    tmp <- NULL
    for (i in 1:N) {
      if (x[i] < 0) 
        tmp <- c(tmp, y[i])
    }
    return (tmp)
  }

  method2 <- function(x,y) {
    return (y[x < 0])
  }

  s1 <- system.time(ans1 <- method1(x,y))
  s2 <- system.time(ans2 <- method2(x,y))
  print(s1)
  print(s2)
  all.equal(ans1,ans2)
  print(s1/s2)          
  # On my phone it's 5000x faster
  return (0)
}

if (identical (environment (), globalenv ()))
  quit (status = main(commandArgs(trailingOnly = TRUE)))

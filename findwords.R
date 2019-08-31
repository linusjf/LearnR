#!/usr/bin/env Rscript
findwords <- function(tf) { 
  # read in the words from the file,
  # into a vector of mode character
  txt <- scan(tf,"")
  wl <- list() 
  for (i in 1: length(txt)) { 
    wrd <- txt[i] # ith word in input file
    wl[[wrd]] <- c(wl[[wrd]], i) 
  } 
  return( wl) 
}

# sorts wrdlst, 
# the output of findwords()
# alphabetically by word 
alphawl <- function( wrdlst) { 
  nms <- names(wrdlst) 
  # the words 
  sn <- sort(nms) 
  # same words in alpha order 
  return (wrdlst[sn]) 
  # return rearranged version 
}

# orders the output of findwords() 
# by word frequency 
freqwl <- function(wrdlst) { 
  freqs <- sapply(wrdlst, length) 
  # get word frequencies 
  return(wrdlst[order(freqs)])
}


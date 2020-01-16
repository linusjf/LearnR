#!/usr/bin/env Rscript
findwords <- function(tf) {
  # read in the words from the file, into a vector of mode character
  txt <- scan(tf, "")
  words <- split(1:length(txt), txt)
  return(words)
}

# sorts wrdlst, the output of findwords() alphabetically by word
alphawl <- function(wrdlst) {
  nms <- names(wrdlst)
  # the words
  sn <- sort(nms)
  # same words in alpha order
  return(wrdlst[sn])
  # return rearranged version
}

# orders the output of findwords() by word frequency
freqwl <- function(wrdlst) {
  freqs <- sapply(wrdlst, length)
  # get word frequencies
  return(wrdlst[order(freqs)])
}

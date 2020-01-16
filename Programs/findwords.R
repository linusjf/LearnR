#!/usr/bin/env Rscript
findwords <- function(tf) {
  # read in the words from the file,
  # into a vector of mode character
  txt <- scan(tf, "")
  words <- split(seq_len(length(txt)), txt)
  return(words)
}

# sorts wrdlst, the output of findwords() alphabetically by word
alphawl <- function(wrdlst) {
  # the words
  nms <- names(wrdlst)
  # same words in alpha order
  sn <- sort(nms)
  # return rearranged version
  return(wrdlst[sn])
}

# orders the output of findwords() by word frequency
freqwl <- function(wrdlst) {
  # get word frequencies
  freqs <- sapply(wrdlst, length)
  return(wrdlst[order(freqs)])
}

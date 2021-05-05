#!/usr/bin/env Rscript

# Comment: Code by Steve Miller (Graduate student in the Statistics Department,
# The University of Auckland).  An example of a one-off image using the
# traditional graphics system.  All parameters are hard-coded and the image only
# looks right when drawn with a specific aspect ratio (4:1).  Also an example of
# drawing an empty plot with a specific coordinate system and then building up a
# final image by drawing individual lines and and pieces of text.  Small point of
# interest is the use of some special glyphs (e.g., treble clef) from the Hershey
# vector fonts.

main <- function(argv) {

  # TOP: music
  par(yaxt = "n", xaxt = "n", ann = F, fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), 
    cex = 0.5)
  plot(1:10, type = "n", xlab = "", ylab = "")
  title(main = "A Little Culture", line = -1)
  e <- 5
  f <- 5.2
  g <- 5.4
  a <- 5.6
  b <- 5.8
  c <- 6
  d <- 6.2
  f2 <- 6.6

  # stave
  for (i in c(e, g, b, d, f2)) {
    lines(x = c(1, 10), y = rep(i, 2))
  }

  # Hershey characters (treble clef, crotchet rest, sharp)
  s1 <- list(x = 1.2, y = g)  # place clef on G
  text(list(x = c(s1$x, s1$x + 8.5, s1$x + 0.5), y = c(s1$y, s1$y + 0.4, f2)), 
    vfont = c("serif", "plain"), labels = c("\\#H2330", "\\#H2378", "\\#H2323"), 
    cex = 2)

  # time signature
  text(x = rep(s1$x + 0.3, 2), y = c(s1$y, s1$y + 0.8), labels = c("4", "4"), cex = 0.8)

  # notes
  points(list(y = c(f, a, g, a, b, b, b), x = c(s1$x + 1, s1$x + 2, s1$x + 3, s1$x + 
    4, s1$x + 5.5, s1$x + 6.5, s1$x + 7.5)), pch = 16, cex = 1.2)

  # note tails
  tail <- 1.05
  for (n in c(b, a, g, a)) {
    lines(x = rep(s1$x + tail, 2), y = c(n, n + 1))
    tail <- tail + 1
  }

  tail <- tail + 0.5
  for (n in c(b, b, b)) {
    lines(x = rep(s1$x + tail, 2), y = c(n, n + 1))
    tail <- tail + 1
  }

  # bar lines
  lines(x = rep(1, 2), y = c(e, f2))
  lines(x = rep(s1$x + 4.75, 2), y = c(e, f2))
  lines(x = rep(9.9, 2), y = c(e, f2))
  lines(x = rep(10, 2), y = c(e, f2), lwd = 2)

  # lyrics
  text(x = seq(s1$x + 1, s1$x + 8.5, by = 0.5), y = rep(4, 16), labels = c("Ma-", 
    "", "ry", "", "had", "", "a", "", "", "lit-", "", "tle", "", "lamb", "", 
    ""), cex = 1, font = 4)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

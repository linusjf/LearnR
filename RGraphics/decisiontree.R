#!/usr/bin/env Rscript
suppressMessages(library(callr))
suppressMessages(library(grid))
suppressMessages(library(partykit))
suppressMessages(library(ipred))
suppressMessages(library(TH.data))
data("GlaucomaM")

main <- function(argv) {
  # CLASSIFICATION
  # fitting
  glau <- TH.data::GlaucomaM
  print(head(glau))
  levels(glau$Class) <- c("glau", "norm")
  fm_class <- partykit::ctree(Class ~ ., data = glau)

  # visualization
  grid::pushViewport(grid::viewport(gp = grid::gpar(cex = 0.6)))
  plot(fm_class, new = FALSE, terminal.panel = partykit::myNode)
  grid::popViewport()
  print(fm_class$terms)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

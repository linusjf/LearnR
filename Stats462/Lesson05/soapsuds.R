#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table("../Data/soapsuds.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  data %<>%
    mutate(so.su = soap * suds) %>%
    mutate(soap.sq = soap * soap)
  n <- nrow(data)
  sumx <- sum(data$soap)
  sumxy <- sum(data$so.su)
  sumxsquare <- sum(data$soap.sq)
  sumy <- sum(data$suds)
  `X'X` <- matrix(c(n, sumx,
                    sumx, sumxsquare),
  nrow = 2, ncol = 2, byrow = TRUE)
  `X'Y` <- matrix(c(sumy, sumxy),
  nrow = 2, ncol = 1, byrow = TRUE)
  inverse <- solve(`X'X`)
  b <- inverse %*% `X'Y`
  rownames(b) <- c("b0", "b1")
  colnames(b) <- c("Beta Hat")
  print(b)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

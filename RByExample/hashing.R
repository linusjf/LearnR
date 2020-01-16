#!/usr/bin/env Rscript

library(xtable)
# Goal: Associative arrays (as in awk) or hashes (as in perl).  Or, more
# generally, adventures in R addressing.

main <- function(argv) {
  # Here's a plain R vector:
  print("x <- c(2,3,7,9)")
  x <- c(2, 3, 7, 9)
  # But now I tag every elem with labels:
  print("names(x) <- c(\"kal\",\"sho\",\"sad\",\"aja\")")
  names(x) <- c("kal", "sho", "sad", "aja")
  # Associative array operations:
  print("x[\"kal\"] <- 12")
  x["kal"] <- 12
  # Pretty printing the entire associative array:
  print(x)

  print("m <- matrix(runif(10), nrow=5")
  # This works for matrices too:
  m <- matrix(runif(10), nrow = 5)
  print("rownames(m) <- c(\"violet\",\"indigo\",
  \"blue\",\"green\",\"yellow\")")
  rownames(m) <- c("violet", "indigo", "blue", "green", "yellow")
  print("colnames(m) <- c(\"Asia\",\"Africa\")")
  colnames(m) <- c("Asia", "Africa")
  # The full matrix --
  print(m)
  # Or even better --
  print("xtable(m)")
  print(xtable::xtable(m))

  # Now address symbolically
  print("m[,\"Africa\"]")
  print(m[, "Africa"])
  print("m[\"indigo\",]")
  print(m["indigo", ])
  print("m[\"indigo\",\"Africa\"]")
  print(m["indigo", "Africa"])

  # The 'in' operator, as in awk --
  print("Using 'in' operator on 'yellow,orange,red'")
  for (colour in c("yellow", "orange", "red")) {
    if (colour %in% rownames(m)) {
      cat("For Africa and ", colour, " we have ", m[colour, "Africa"], "\n")
    } else {
      cat("Colour ", colour, " does not exist in the hash.\n")
    }
  }

  # This works for data frames also --
  print("d <- data.frame(m)")
  d <- data.frame(m)
  print("d")
  print(d)
  # Look closely at what happened --
  print("str(d)")
  str(d)
  # The colours are the rownames(d).

  # Operations --
  print("d$Africa")
  print(d$Africa)
  print("d[,\"Africa\"]")
  print(d[, "Africa"])
  print("d[\"yellow\",]")
  print(d["yellow", ])
  # or
  print("subset(d, rownames(d)==\"yellow\"")
  print(subset(d, rownames(d) == "yellow"))

  print("colnames(d) <- c(\"Antarctica\",\"America\")")
  print(colnames(d) <- c("Antarctica", "America"))
  print("d")
  print(d)
  print("d$America")
  print(d$America)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

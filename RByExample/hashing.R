#!/usr/bin/env Rscript

brary(xtable)
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
    print(xtable(m))
    
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
        if (colour %in% rownames(m)) 
            cat("For Africa and ", colour, " we have ", m[colour, "Africa"], "\n") else cat("Colour ", colour, " does not exist in the hash.\n")
    }
    
    # This works for data frames also --
    print("D <- data.frame(m)")
    D <- data.frame(m)
    print("D")
    print(D)
    # Look closely at what happened --
    print("str(D)")
    str(D)
    # The colours are the rownames(D).
    
    # Operations --
    print("D$Africa")
    print(D$Africa)
    print("D[,\"Africa\"]")
    print(D[, "Africa"])
    print("D[\"yellow\",]")
    print(D["yellow", ])
    # or
    print("subset(D, rownames(D)==\"yellow\"")
    print(subset(D, rownames(D) == "yellow"))
    
    print("colnames(D) <- c(\"Antarctica\",\"America\")")
    print(colnames(D) <- c("Antarctica", "America"))
    print("D")
    print(D)
    print("D$America")
    print(D$America)
}

if (identical(environment(), globalenv())) {
    quit(status = main(commandArgs(trailingOnly = TRUE)))
}

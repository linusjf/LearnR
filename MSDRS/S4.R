#!/usr/bin/env Rscript

main <- function(argv) {
  setClass("bus_S4",
    slots = list(
      n_seats = "numeric",
      top_speed = "numeric",
      current_speed = "numeric",
      brand = "character"
    )
  )
  setClass("party_bus_S4",
    slots = list(
      n_subwoofers = "numeric",
      smoke_machine_on = "logical"
    ),
    contains = "bus_S4"
  )
  my_bus <- new("bus_S4",
    n_seats = 20, top_speed = 80,
    current_speed = 0, brand = "Volvo"
  )
  print(my_bus)
  my_party_bus <- new("party_bus_S4",
    n_seats = 10, top_speed = 100,
    current_speed = 0, brand = "Mercedes-Benz",
    n_subwoofers = 2, smoke_machine_on = FALSE
  )
  print(my_party_bus)
  print(my_bus@n_seats)
  print(my_party_bus@top_speed)

  is_bus_moving <- setGeneric("is_bus_moving", function(x) {
    standardGeneric("is_bus_moving")
  })

  setMethod(
    "is_bus_moving",
    c(x = "bus_S4"),
    function(x) {
      x@current_speed > 0
    }
  )
  print(is_bus_moving(my_bus))
  my_bus@current_speed <- 1
  print(is_bus_moving(my_bus))
  setGeneric("print")
  setMethod(
    "print",
    c(x = "bus_S4"),
    function(x) {
      cat(paste(
        "This", x@brand, "bus is traveling at a speed of",
        x@current_speed
      ), "\n")
    }
  )
  print(my_bus)
  print(my_party_bus)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

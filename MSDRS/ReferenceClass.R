#!/usr/bin/env Rscript

main <- function(argv) {
  student <- setRefClass(
    "student",
    fields =
      list(
        name = "character",
        grad_year = "numeric",
        credits = "numeric",
        id = "character",
        courses = "list"
      ),
    # nolint start
    methods =
      list(
        hello = function() {
          paste("Hi! My name is", .self$name)
        },
        add_credits = function(n) {
          .self$credits <- .self$credits + n
        },
        get_email = function() {
          paste0(.self$id, "@jhu.edu")
        }
      )
  )
  # nolint end
  brooke <- student$new(
    name = "Brooke",
    grad_year = 2019,
    credits = 40,
    id = "ba123",
    courses = list(
      "Ecology",
      "Calculus III"
    )
  )

  roger <- student$new(
    name = "Roger",
    grad_year = 2020,
    credits = 10,
    id = "rp456",
    courses = list(
      "Puppetry",
      "Elementary Algebra"
    )
  )
  print(brooke)
  print(roger)
  print(brooke$credits)
  print(roger$hello())
  print(roger$get_email())
  print(brooke$credits)
  brooke$add_credits(4)
  print(brooke$credits)

  grad_student <- setRefClass(
    "grad_student",
    contains =
      "student",
    fields =
      list(
        thesis_topic = "character"
      ),
    # nolint start
    methods =
      list(
        defend = function() {
          paste0(.self$thesis_topic, ". QED.")
        }
      )
  )

  # nolint end
  jeff <- grad_student$new(
    name = "Jeff", grad_year = 2021, credits = 8,
    id = "jl55", courses = list(
      "Fitbit Repair",
      "Advanced Base Graphics"
    ),
    thesis_topic = "Batch Effects"
  )

  print(jeff$defend())
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

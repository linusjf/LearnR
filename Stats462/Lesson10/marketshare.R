#!/usr/bin/env Rscript
marketshare.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/marketshare.txt"
  )
}

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
source(lib_path())

main <- function(argv) {
  data <- read.table(marketshare.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  attach(data)
  model.1 <- lm(MarketShare ~ Price + P1 + P2, data)
  print(summary(model.1))
  print(anova(model.1))
  eqn1 <- model_equation(model.1, digits = 4)
  
  detach(data)
  return(0)
}

plot_models <- function(data) {

  model.2 <- lm(Progeny ~ Parent, weights = 1 / SD^2, data)
  summary(model.2)
  eqn2 <- model_equation(model.2, digits = 4)

  plot(
    x = Parent, y = Progeny, ylim = c(0.158, 0.174),
    panel.last = c(
      lines(sort(Parent), fitted(model.1)[order(Parent)], col = "blue"),
      lines(sort(Parent), fitted(model.2)[order(Parent)], col = "red")
    ),
      col.main = "blue",
      col.sub = "red",
      main = eqn1,
      sub = eqn2
  )
  legend("topleft",
    col = c("blue", "red"), lty = 1,
    inset = 0.02, legend = c("OLS", "WLS")
  )
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

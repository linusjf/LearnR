# A fun welcome message
message(paste("Hi ", Sys.info()["user"], ", Welcome to R"))
# Customise the R prompt that prefixes every command
# (use " " for a blank prompt)
options(prompt = "> ")

# General options
options(tab.width = 2)
options(width = 80)
options(graphics.record = TRUE)

# nolint start
.First <- function() {
  cat("\nWelcome at", date(), "\n")
}

.Last <- function() {
  cat("\nGoodbye at ", date(), "\n")
}
# nolint end
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  options(repos = r)
})
if (interactive()) {
  try(fortunes::fortune(), silent = TRUE)
}

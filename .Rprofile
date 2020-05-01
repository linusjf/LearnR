# A fun welcome message
if (interactive()) {
  try(startup::startup())
  message(paste("Hi ", Sys.info()["user"], ", Welcome to R"))
  # Customise the R prompt that prefixes every command
  # (use " " for a blank prompt)
  options(prompt = "> ")
}

options(Ncpus = 6)
if (interactive()) {
  # General options
  options(tab.width = 2)
  options(width = 80)
  options(graphics.record = TRUE)
}

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
  rm(r)
})

# nolint start
.First <- function() {
  if (interactive()) {
    cat("\nWelcome at", date(), "\n")
  }
}

.Last <- function() {
  if (interactive()) {
    cat("\nGoodbye at ", date(), "\n")
  }
}
# nolint end

if (interactive()) {
  try(fortunes::fortune(), silent = TRUE)
}

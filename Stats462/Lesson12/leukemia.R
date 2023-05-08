#!/usr/bin/env Rscript
leukemia.txt <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/leukemia_remission.txt"
  )
}, list())

libfunc <- do.call(function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}, list())

library(skimr)
source(libfunc)
suppressPackageStartupMessages(library(survey))
suppressPackageStartupMessages(library(glmulti))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(finalfit))
suppressPackageStartupMessages(library(oddsratio))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(ResourceSelection))

main <- function(argv) {
  options(warn = 1)
  data <- read.table(leukemia.txt,
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  full_model <- glm(REMISS ~ .,
    data = data,
    family = "binomial"
  )
  print(full_model)
  print(summary(full_model))
  glmulti.logistic.out <-
    glmulti(DF2formula(data),
      data = data,
      level = 1, # No interaction considered
      method = "h", # Exhaustive approach
      crit = "bic", # BIC as criteria
      confsetsize = 1, # Keep 1 best models
      plotty = FALSE, report = FALSE, # No plot or interim reports
      fitfunction = "glm", # glm function
      family = binomial
    ) # binomial family for logistic regression
  print(glmulti.logistic.out@formulas)
  model <- glmulti.logistic.out@objects[[1]]
  print(summary(model))
  result <- regTermTest(model,
    formula(model),
    method = "Wald"
  )
  print(result)
  data %<>%
    mutate(fitted = model$fitted.values) %>%
    mutate(linear.fitted = model$linear.predictors)
  #    arrange(LI)
  eqn <- logit_model_equation(model, digits = 4)
  with(data, {
    plot(
      x = LI, fitted,
      xlab = "LI",
      ylab = "probability of event",
      col = "red",
      ylim = c(-0.001, 1.001),
      main = paste0(
        "Binary Fitted Line Plot\n",
        eqn
      )
    )
    curve(
      predict(
        model,
        newdata =
          data.frame(LI = LI),
        type = "response"
      ),
      add = TRUE, xname = "LI"
    )
    points(LI, REMISS, pch = 15, col = "blue")
  })
  with(data, {
    plot(
      x = LI, exp(linear.fitted),
      xlab = "LI", ylab = "Odds", col = "red",
      main = "Odds Plot"
    )
    curve(
      exp(predict(
        model,
        newdata =
          data.frame(LI = LI)
      )),
      add = TRUE, xname = "LI"
    )
  })
  with(data, {
    plot(LI, linear.fitted,
      xlab = "LI", ylab = "Odds", col = "red",
      main = "Log Odds Plot"
    )
    curve(
      predict(
        model,
        newdata =
          data.frame(LI = LI)
      ),
      add = TRUE, xname = "LI"
    )
  })
  explanatory <- c("LI")
  dependent <- "REMISS"
  data %>%
    or_plot(dependent, explanatory)
  print(as.data.frame(or_glm(
    data = data,
    model = model,
    incr = list(LI = 0.1)
  )))
  print(as.data.frame(or_glm(
    data = data,
    model = model,
    incr = list(LI = 1)
  )))
  null_model <- glm(REMISS ~ 1,
    data = data,
    family = "binomial"
  )
  print(lrtest(null_model, model))
  print(null_model$deviance - model$deviance)
  with(
    data,
    print(hoslem.test(REMISS, fitted))
  )
  with(summary(model), {
    print("R-squared")
    print(1 - deviance / null.deviance)
  })
  data %<>%
    mutate(raw.residuals = REMISS - fitted) %>%
    mutate(pearson.residuals = raw.residuals /
      sqrt(fitted * (1 - fitted)))
  deviance.residuals <- NULL
  with(data, {
    term <- ifelse(data$REMISS == 1,
      REMISS * log(REMISS / fitted),
      (1 - REMISS) * log((1 - REMISS) / (1 - fitted))
    )
    full_term <- 2 * term
    deviance.residuals <<- sign(REMISS - fitted) *
      sqrt(full_term)
  })
  data %<>%
    mutate(deviance.residuals = deviance.residuals)
  obs_count <- nrow(data)
  plot(seq_len(obs_count), data$pearson.residuals,
    ylab = "Pearson residual",
    xlab = "Observation order",
    main = "Versus order",
    sub = "(response is REMISS)",
    ylim = c(-3, 3), type = "b", pch = 15, col = "blue"
  )
  abline(h = 0, col = "red")
  plot(seq_len(obs_count), data$deviance.residuals,
    ylab = "Deviance residual",
    xlab = "Observation order",
    main = "Versus order",
    sub = "(response is REMISS)",
    ylim = c(-3, 3), type = "b", pch = 15, col = "blue"
  )
  abline(h = 0, col = "red")
  hats <- calculate_hat(model, data)
  data %<>%
    mutate(hat.values = hats)
  p <- length(model$coefficients)
  n <- nrow(data)
  plot(seq_len(obs_count), data$hat.values,
    ylab = "Hat values",
    xlab = "Observation order",
    main = "Versus order",
    sub = "(response is REMISS)",
    type = "b", pch = 15, col = "blue",
    ylim = c(
      min(hats, 2 * p / n),
      max(hats, 3 * p / n)
    )
  )
  abline(h = 3 * p / n, col = "red")
  abline(h = 2 * p / n, col = "orange")

  data %<>%
    mutate(
      studentized.pearson =
        pearson.residuals / sqrt(1 - hat.values)
    ) %>%
    mutate(
      studentized.deviance =
        deviance.residuals / sqrt(1 - hat.values)
    ) %>%
    mutate(
      cooks.distance =
        (pearson.residuals^2 * hat.values) / (p * (1 - hat.values)^2)
    )

  df <- data %>%
    filter(abs(pearson.residuals) > 2)
  print(df)

  plot(seq_len(obs_count), data$studentized.pearson,
    ylab = "Studentized pearson residuals",
    xlab = "Observation order",
    main = "Versus order",
    sub = "(response is REMISS)",
    type = "b", pch = 15, col = "blue",
    ylim = c(
      min(data$studentized.pearson, -3),
      max(data$studentized.pearson, 3)
    )
  )
  abline(h = 3, col = "red")
  abline(h = -3, col = "red")
  abline(h = 2, col = "orange")
  abline(h = -2, col = "orange")

  plot(seq_len(obs_count), data$studentized.deviance,
    ylab = "Studentized deviance residuals",
    xlab = "Observation order",
    main = "Versus order",
    sub = "(response is REMISS)",
    type = "b", pch = 15, col = "blue",
    ylim = c(
      min(data$studentized.deviance, -3),
      max(data$studentized.deviance, 3)
    )
  )
  abline(h = 3, col = "red")
  abline(h = -3, col = "red")
  abline(h = 2, col = "orange")
  abline(h = -2, col = "orange")

  data %<>%
    mutate(cumsum_hat = cumsum(hat.values / (1 - hat.values)))
  mean_hat <- sum(data$cumsum_hat) / (n * (n + 1) / 2)
  cooks.cutoff <- mean_hat *
    qchisq(0.95, 1)
  plot(seq_len(obs_count), data$cooks.distance,
    ylab = "Cooks distance",
    xlab = "Observation order",
    main = "Versus order",
    sub = "(response is REMISS)",
    type = "b", pch = 15, col = "blue",
    ylim = c(
      min(data$cooks.distance),
      max(data$cooks.distance, cooks.cutoff)
    )
  )
  abline(h = cooks.cutoff, col = "red")
  abline(h = 4 / (n - p), col = "pink")
  abline(h = 4 / n, col = "orange")
  plot(seq_len(obs_count), pf(data$cooks.distance, p, n - p),
    ylab = "F distribution prob",
    xlab = "Observation order",
    main = "Versus order",
    sub = "(response is REMISS)",
    type = "b", pch = 15, col = "blue",
    ylim = c(0, max(0.5, pf(data$cooks.distance, p, n - p)))
  )
  abline(h = 0.5, col = "red")
  return(0)
}

calculate_hat <- function(model, data) {
  v <- model$fitted.values * (1 - model$fitted.values)
  X <- model.matrix(model)
  n <- nrow(data)
  W <- diag(v, n, n)
  Xt <- t(X)
  XtWX <- Xt %*% W %*% X
  XtWXinverse <- solve(XtWX)
  HAT <- vector(mode = "numeric", length = n)
  for (idx in seq_len(n)) {
    xi <- matrix(X[idx, ], nrow = 2)
    xit <- t(xi)
    HAT[idx] <- v[idx] * (xit %*% XtWXinverse %*% xi)
  }
  return(HAT)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

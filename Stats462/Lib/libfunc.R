
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(alr3))
suppressPackageStartupMessages(library(regclass))
library(HoRM)

rad2deg <- function(rad) {
  (rad * 180) / (pi)
}

deg2rad <- function(deg) {
  (deg * pi) / (180)
}

vif_factors <- function(model) {
  VIF(model)
}

model_equation <- function(model, ...) {
  format_args <- list(...)

  model_coeff <- model$coefficients
  model_coeff <- model_coeff[!is.na(model_coeff)]
  format_args$x <- abs(model_coeff)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1] < 0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}

scatterplot_matrix <- function(data, title) {
  cols <- colnames(data)
  fields <- "~"
  for (name in cols) {
    fields <- paste0(fields, name, " + ")
  }
  fields <- substring(fields, 1, nchar(fields) - 2)
  pairs(eval(parse(text = fields)),
    data = data,
    lower.panel = panel.smooth, upper.panel = panel_cor,
    pch = 20, main = title
  )
}

hildreth_lu <- function(y, x, rho) {
  models <- vector("list", length(rho))
  sses <- c()
  i <- 1
  for (value in rho) {
    model <- hildreth.lu(y,
                         x,
                         value)
    models[[i]] <- model
    sses <- c(sses, sigma(model))
    i <- i + 1
  }
  idx <- which(sses == min(sses))
  return(models[c(idx)])
}

# panel.smooth function is built in.
# panel_cor puts correlation in upper panels, size proportional to correlation
panel_cor <- function(x, y, digits = 2, prefix = "",
                      cex_cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789),
    digits = digits
  )[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex_cor)) {
    cex_cor <- 0.8 / strwidth(txt)
  }
  text(0.5, 0.5, txt, cex = cex_cor * r)
}

#################################################################
###
### Functions to calculate Predictive R-squared
###
#####################################################################


### This calculate the PRESS (predictive residual sum of squares), the lower,
### the better

#' @title PRESS
#' @author Thomas Hopper
#' @description Returns the PRESS statistic (predictive residual sum of
#' squares).
#'              Useful for evaluating predictive power of regression models.
#' @param linear.model A linear regression model (class 'lm'). Required.
# nolint start
PRESS <- function(linear_model) {
  #' calculate the predictive residuals
  pr <- residuals(linear_model) /
    (1 - lm.influence(linear_model)$hat)

  #' calculate the PRESS
  PRESS <- sum(pr^2)

  return(PRESS)
}
# nolint end


### This calculates the Predictive r-squared

#' @title Predictive R-squared
#' @author Thomas Hopper
#' @description returns the predictive r-squared. Requires the function PRESS(),
#' which returns
#'              the PRESS statistic.
#' @param linear.model A linear regression model (class 'lm'). Required.
#'
pred_r_squared <- function(linear_model) {
  #' Use anova() to get the sum of squares for the linear model
  lm_anova <- anova(linear_model)
  #' Calculate the total sum of squares
  tss <- sum(lm_anova$`Sum Sq`)
  # Calculate the predictive R^2
  pred_r_squared <- 1 - PRESS(linear_model) / tss

  return(pred_r_squared)
}


### This calculate the R-squared, the adj-R-squared and the Predictive R-squared
### (from the functions above)

#' @title Model Fit Statistics
#' @description Returns lm model fit statistics R-squared, adjucted R-squared,
#'      predicted R-squared and PRESS.
#'      Thanks to John Mount for his 6-June-2014 blog post, R style tip: prefer
#'      functions that return data frames" for
#'      the idea
# nolint start
#' \link{http://www.win-vector.com/blog/2014/06/r-style-tip-prefer-functions-that-return-data-frames}
# nolint end
#' @return Returns a data frame with one row and a column for each statistic
#' @param linear.model A \code{lm()} model.
model_fit_stats <- function(linear_model) {
  if (class(linear_model) != "lm") {
    stop("Not an object of class 'lm' ")
  }
  summ <- summary(linear_model)
  sigma <- summ$sigma
  r_sqr <- summ$r.squared
  adj_r_sqr <- summ$adj.r.squared
  ratio_adjr2_to_r2 <- (adj_r_sqr / r_sqr)
  pre_r_sqr <- pred_r_squared(linear_model)
  press <- PRESS(linear_model)
  p_value <- lmp(linear_model)
  return_df <-
    data.frame(
      "Sigma" = sigma,
      "R.squared" = r_sqr,
      "Adj.R.squared" = adj_r_sqr,
      "Ratio.Adj.R2.to.R2" = ratio_adjr2_to_r2,
      "Pred.R.squared" = pre_r_sqr,
      "PRESS" = press,
      "p.value" = p_value
    )
  return(round(return_df, 3))
}

model_coeffs <- function(reg) {
  if (class(reg) != "lm") {
    stop("Not an object of class 'lm' ")
  }
  params <- reg$coefficients
  names <- names(params)
  p_names <- paste0(names, ".p")
  p_values <- summary(reg)$coefficients[, 4]
  names <- c(names, p_names)
  params <- c(params, p_values)
  names(params) <- names
  # add vif only if more than 1 predictor
  if (length(reg$coefficients) > 2) {
  vif_values <- vif_factors(reg)
  vif_names <- names(vif_values)
  vif_names <- paste0(vif_names, ".vif")
  names <- c(names, vif_names)
  params <- c(params, vif_values)
  names(params) <- names
  }
  names <- stringr::str_sort(names)
  sorted_params <- c()
  for (name in names) {
    sorted_params[name] <- params[name]
  }
  return_df <-
    as.data.frame(do.call(cbind, as.list(sorted_params)))
  return(round(return_df, 3))
}

lmp <- function(modelobject) {
  if (class(modelobject) != "lm") {
    stop("Not an object of class 'lm' ")
  }
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  attributes(p) <- NULL
  return(p)
}

complete_anova <- function(lm) {
  if (class(lm) != "lm") {
    stop("Not an object of class 'lm' ")
  }
  pure <- alr3::pureErrorAnova(lm)
  return(pure)
}

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(olsrr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(regclass))
suppressPackageStartupMessages(library(purrr))

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

source(lib_path())

validate <- function(...) {
  parms <- list(...)
  if (is.null(parms$data)) {
    stop("Data frame expected. Cannot be NULL")
  }
  if (is.null(parms$response) | parms$response == "") {
    stop("Response cannot be NULL or empty")
  }
  if (!parms$response %in% colnames(parms$data)) {
    stop(sprintf("Column %s does not exist", parms$response))
  }
  if (!is.null(parms$predictors) & length(parms$predictors) > 0) {
    for (value in parms$predictors) {
      if (!value %in% colnames(parms$data)) {
        stop(sprintf("Column %s does not exist", value))
      }
    }
  }
  if (parms$alpha_removal < 0 | parms$alpha_removal > 1) {
    stop("alpha_removal is between 0 and 1 included")
  }
  if (parms$alpha_entry < 0 | parms$alpha_entry > 1) {
    stop("alpha_entry is between 0 and 1 included")
  }
}

get_predictors <- function(data, response, predictors, removals) {
  if (is.null(predictors)) {
    predictors <- colnames(data)
  }
  predictors <- predictors[predictors != response]
  if (!is.null(removals)) {
    predictors <- predictors[!predictors %in% removals]
  }
  return(predictors)
}

get_model_variables <- function(formula) {
  model_variables <- c()
  if (!is.null(formula)) {
      term <-
           terms(formula, keep.order = TRUE)
  model_variables <- attr(term, "term.labels")
  }
  return(model_variables)
}

get_models <- function(data, response, predictors, model_variables) {
  i <- 1
  models <- list()
  for (value in predictors) {
    variables <- c(model_variables, value)
    f <-
      as.formula(paste(response,
                       paste(variables, collapse = " + "),
                       sep = " ~ "))
    model <- lm(f, data)
    models[[i]] <- model
    i <- i + 1
  }
  return(models)
}

collect_p <- function(models) {
  p_vals <- c()
  for (model in models) {
    index <- length(model$coefficients)
    summ <- summary(model)
    p <- summ$coefficients[index, 4]
    p_vals <- c(p_vals, p)
  }
  return(p_vals)
}

finalize_model <- function(data, formula, alpha_entry) {
    print(sprintf(
      "No p-value meets criteria of alpha entry < %f",
      alpha_entry
    ))
    if (!is.null(formula)) {
      print("Final Model chosen: ")
      model <- lm(formula, data)
      print(model_equation(model, digits = 4))
      return(model)
    }
    else
      return(NULL)
}

choose_model <- function(p_vals, models) {
    p_min <- min(p_vals)
    p_min_index <- which(p_vals == p_min)
    model_chosen <- models[[p_min_index]]
    print("Model chosen currently: ")
    print(model_equation(model_chosen, digits = 4))
    return(model_chosen)
}

check_impact_p <- function(model_chosen, alpha_removal) {

    formula <- formula(model_chosen)
    terms.object <- terms(formula)
    response_index <- attr(terms.object, "response")
    factors <- attr(terms.object, "factors")
    response <- rownames(factors)[response_index]

    print("Checking for impact on p-values")

    summ <- summary(model_chosen)
    coefficients <-
      as.data.frame(summ$coefficients)
    colnames(coefficients) <-
      c("Beta", "SE", "t.val", "p.val")
    coefficients <- tail(coefficients, -1)
    print(coefficients)
    drop <- coefficients %>%
      rownames_to_column("rownames") %>%
      filter(p.val > alpha_removal) %>%
      column_to_rownames("rownames")
    if (nrow(drop) > 0) {
    retain <- coefficients %>%
      rownames_to_column("rownames") %>%
      filter(p.val <= alpha_removal) %>%
      column_to_rownames("rownames")
    retain <-
      rownames(retain)
    formula <-
      as.formula(paste(response,
                       paste(retain, collapse = " + "),
                       sep = " ~ "))
    }
    return(formula)
}


step_wise_regression <- function(data,
                                 response,
                                 predictors = NULL,
                                 removals = NULL,
                                 formula = NULL,
                                 alpha_removal = 0.15,
                                 alpha_entry = 0.15
                                 ) {
  validate(
    data = data, response = response, predictors = predictors, removals =
      removals, alpha_removal = alpha_removal, alpha_entry = alpha_entry
  )
  predictors <- get_predictors(data, response, predictors, removals)
  model_variables <- get_model_variables(formula)

  models <- get_models(data, response, predictors, model_variables)

  p_vals <- collect_p(models)

  # is p >= alpha entry criteria
  if (min(p_vals) >= alpha_entry) {
      final <- finalize_model(data, formula, alpha_entry)
      if (is.null(final))
        return(lm(paste0(response, " ~ ."),
                  data))
      else return(final)
  } else {
    model_chosen <- choose_model(p_vals, models)

    formula <- formula(model_chosen)
    model_variables <- attr(terms(formula), "term.labels")
    predictors <- predictors[!predictors %in% model_variables]
    removals <- c(removals, model_variables)

    formula <- check_impact_p(model_chosen, alpha_removal)

    return(step_wise_regression(data, response,
    predictors, removals, formula,
    alpha_removal,
    alpha_entry))
    }
}

best_vif <- function(models, cutoff = 4) {
  selected <- list()
  nvars <- c()
  for (model in models) {
    vif <- NULL
    if (length(model$coefficients) > 2) {
      vif <- VIF(model)
    } else {
      vif <- 1
    }
    coeffs <- names(model$coefficients)[2:length(model$coefficients)]
    df <- data.frame(list(coeffs = coeffs, vif = vif))
    df %<>%
      filter(vif > cutoff)
    if (nrow(df) == 0) {
      nvars <- c(nvars, length(coeffs))
      selected[[length(selected) + 1]] <- model
    }
  }
  if (length(selected) <= 1) {
    return(selected)
  } else {
    # pick models with least variables
    min_nvars <- min(nvars)
    index <- 1
    for (model in selected) {
      if ((length(model$coefficients) - 1) != min_nvars) {
        selected[[index]] <- NULL
        index <- index + 1
      }
    }
    return(compact(selected))
  }
}

compute_cp <- function(full_model, model) {
  res <- NULL
  if (length(full_model$coefficients) <
    length(model$coefficients)) {
    res <- anova(model, full_model)
  } else {
    res <- anova(full_model, model)
  }
  res %<>%
    mutate(mse = RSS / Res.Df)
  n <- nrow(model$model)
  p <- length(model$coefficients)
  ssek <- res$RSS[2]
  mseall <- res$mse[1]
  cp <- ssek / mseall +
    2 * p - n
  return(cp)
}

fill_models <- function(models, model) {
  if (!inherits(models, c("ols_step_best_subset",
                          "ols_step_all_possible"))) {
    stop("Class has to be ols_step_best_subset or
         ols_step_all_possible")
  }
  lmset <- list()
  nrows <- nrow(models)
  for (idx in seq_len(nrows)) {
    df <- models %>%
      filter(mindex == idx)
    predictors <- df$predictors
    rhs <- str_replace_all(predictors, " ", "+")
    formula <- update(formula(model), paste0(". ~ ", rhs))
    data <- model$model
    lmset[[length(lmset) + 1]] <-
      lm(formula, data)
  }
  return(lmset)
}

best_model_rsquare <- function(models, model, rsqinc = 0.05) {
  if (!inherits(models, c("ols_step_best_subset",
                          "ols_step_all_possible"))) {
    stop("Class has to be ols_step_best_subset or
         ols_step_all_possible") }
  models %<>%
    mutate(rsq.inc = ((rsquare / lag(rsquare) - 1)))
  models %<>%
    filter(rsq.inc >= rsqinc) %>%
    filter(n == min(n))
  predictors <- models$predictors
  rhs <- str_replace_all(predictors, " ", "+")
  formula <- update(formula(model), paste0(". ~ ", rhs))
  data <- model$model
  return(lm(formula, data))
}

best_model_adjrsquare <- function(models, model) {
  if (!inherits(models, c("ols_step_best_subset",
                          "ols_step_all_possible"))) {
    stop("Class has to be ols_step_best_subset or
         ols_step_all_possible") }
  models %<>%
    filter(adjr == max(adjr))
  predictors <- models$predictors
  rhs <- str_replace_all(predictors, " ", "+")
  formula <- update(formula(model), paste0(". ~ ", rhs))
  data <- model$model
  return(lm(formula, data))
}

best_model_cp <- function(models, model, criteria = "mincp") {
  if (!inherits(models, c("ols_step_best_subset",
                          "ols_step_all_possible"))) {
    stop("Class has to be ols_step_best_subset or
         ols_step_all_possible") }
  data <- model$model
  ncoefs <- length(model$coefficients)
  models %<>%
    mutate(p = n + 1) %>%
    mutate(cpratio = cp / p) %>%
    filter(p != ncoefs)
  if (criteria == "mincp") {
    models %<>%
      filter(cp == min(cp))
  } else if (criteria == "relcp") {
    models %<>%
      filter(cpratio == min(cpratio))
  } else {
    stop("Criteria should be mincp or relcp")
  }
  models %<>%
    filter(n == min(n))
  predictors <- models$predictors
  rhs <- str_replace_all(predictors, " ", "+")
  formula <- update(formula(model), paste0(". ~ ", rhs))
  return(lm(formula, data))
}

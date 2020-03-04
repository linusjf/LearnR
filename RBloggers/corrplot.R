#!/usr/bin/env Rscript
# nolint start
# https://www.r-bloggers.com/correlogram-in-r-how-to-highlight-the-most-correlated-variables-in-a-dataset/
# nolint end
suppressMessages(library(corrplot))

cor_mtest <- function(mat, method) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p_mat <- matrix(NA, n, n)
  diag(p_mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = method)
      p_mat[j, i] <- tmp$p.value
      p_mat[i, j] <- tmp$p.value
    }
  }
  rownames(p_mat) <- colnames(mat)
  colnames(p_mat) <- colnames(mat)
  return(p_mat)
}

corrplot2 <- function(data_,
                      method_ = "pearson",
                      sig_level_ = 0.05,
                      order_ = "original",
                      diag_ = FALSE,
                      type_ = "upper",
                      tl_srt_ = 90,
                      number_font_ = 1,
                      number_cex_ = 1,
                      mar_ = c(0, 0, 0, 0)) {
  data <- data_[complete.cases(data_), ]
  print(head(data))
  mat <- cor(data, method = method_)
  p_mat <- cor_mtest(data, method = method_)
  col <- colorRampPalette(c(
    "#BB4444", "#EE9988", "#FFFFFF", "#77AADD",
    "#4477AA"
  ))
  corrplot(mat,
    method = "color", col = col(200), number.font = number_font_,
    mar = mar_, number.cex = number_cex_,
    type = type_, order = order_,
    addCoef.col = "black",
    # add correlation coefficient
    tl.col = "black", tl.srt = tl_srt_,
    # rotation of text labels
    # combine with significance level
    p.mat = p_mat,
    sig.level = sig_level_,
    insig = "blank",
    # hide correlation coefficiens on the diagonal
    diag = diag_
  )
}

main <- function(argv) {
  corrplot2(
    data_ = mtcars,
    method_ = "pearson",
    sig_level_ = 0.05,
    order_ = "original",
    diag_ = FALSE,
    type_ = "upper",
    tl_srt_ = 75
  )
  corrplot2(
    data_ = mtcars,
    method_ = "pearson",
    sig_level_ = 0.01,
    order_ = "original",
    diag_ = FALSE,
    type_ = "upper",
    tl_srt_ = 75
  )
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

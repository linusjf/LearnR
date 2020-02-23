#!/usr/bin/env Rscript
library(corrplot)

cor_mtest <- function(mat, method) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p_mat <- matrix(NA, n, n)
  diag(p_mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- corrplot::cor.test(mat[, i], mat[, j], method = method)
      p_mat[j, i] <- tmp$p.value
      p_mat[i, j] <- tmp$p.value
    }
  }
  rownames(p_mat) <- colnames(mat)
  colnames(p_mat) <- colnames(mat)
  return(p_mat)
}

corrplot2 <- function(data,
                      method = "pearson",
                      sig_level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl_srt = 90,
                      number_font = 1,
                      number_cex = 1,
                      mar = c(0, 0, 0, 0)) {
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  p_mat <- corrplot::cor.mtest(data, method = method)
  col <- colorRampPalette(c(
    "#BB4444", "#EE9988", "#FFFFFF", "#77AADD",
    "#4477AA"
  ))
  corrplot(mat,
    method = "color", col = col(200), number.font = number_font,
    mar = mar, number.cex = number_cex,
    type = type, order = order,
    addCoef.col = "black", # add correlation coefficient
    tl.col = "black", tl.srt = tl_srt, # rotation of text labels
    # combine with significance level
    p.mat = p_mat, sig.level = sig_level, insig = "blank",
    # hide correlation coefficiens on the diagonal
    diag = diag
  )
}

main <- function(argv) {
  corrplot2(
    data = mtcars,
    method = "pearson",
    sig_level = 0.05,
    order = "original",
    diag = FALSE,
    type = "upper",
    tl_srt = 75
  )
  corrplot2(
    data = mtcars,
    method = "pearson",
    sig_level = 0.01,
    order = "original",
    diag = FALSE,
    type = "upper",
    tl_srt = 75
  )
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

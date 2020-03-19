
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
      pch = 20, main = title)
}

# panel.smooth function is built in.
# panel_cor puts correlation in upper panels, size proportional to correlation
panel_cor <- function(x, y, digits = 2, prefix = "",
                      cex_cor, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789),
                  digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex_cor))
      cex_cor <- 0.8 / strwidth(txt)
    text(0.5, 0.5, txt, cex = cex_cor * r)
}

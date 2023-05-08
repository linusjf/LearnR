#!/usr/bin/env Rscript
#
# Comment:
#
# Some simple ideas as a basis for meta-analysis plots.
#
# The code is modular so that something similar could be achieved
# with different data quite simply.  The actual drawing for these data
# only occurs in the last 10 or so lines of code.
#
library(grid)

main <- function(argv) {
  # The horizontal gap between columns with content
  colgap <- grid::unit(3, "mm")

  # The data for column 1
  #
  # Of course, many other possible ways to represent the data
  # One advantage with this way is that col1$labels can be used
  # directly in the calculation of the column widths for the
  # main table (see below)
  #
  # NOTE:  textGrobs are used here so that the fontface (bold in
  # some cases) is associated with the label.  In this way, the
  # calculation of column widths takes into account the font face.
  col1 <- list(
    labels =
      list(
        textGrob("Centre",
          x = 0, just = "left",
          gp = grid::gpar(fontface = "bold", col = "white")
        ),
        textGrob("Thailand", x = 0, just = "left"),
        textGrob("Philippines", x = 0, just = "left"),
        textGrob("All in situ",
          x = 0, just = "left",
          gp = gpar(fontface = "bold.italic")
        ),
        textGrob("Colombia", x = 0, just = "left"),
        textGrob("Spain", x = 0, just = "left"),
        textGrob("All invasive",
          x = 0, just = "left",
          gp = grid::gpar(fontface = "bold.italic")
        ),
        textGrob("All",
          x = 0, just = "left",
          gp = grid::gpar(fontface = "bold")
        )
      ),
    rows = c(1, 5, 6, 8, 11, 12, 14, 16)
  )

  # Labels in col 1 which are not used to calculate the
  # column width (they spill over into col 2)
  col1plus <- list(
    labels =
      list(
        textGrob("Carcinoma in situ",
          x = 0, just = "left",
          gp = grid::gpar(fontface = "bold.italic")
        ),
        textGrob("Invasive cancer",
          x = 0, just = "left",
          gp = grid::gpar(fontface = "bold.italic")
        )
      ),
    rows = c(4, 10)
  )

  # Data for column 2
  col2 <- list(
    labels =
      list(
        textGrob("Cases",
          x = 1, just = "right",
          gp = grid::gpar(fontface = "bold", col = "white")
        ),
        textGrob("327", x = 1, just = "right"),
        textGrob("319", x = 1, just = "right"),
        textGrob("1462",
          x = 1, just = "right",
          gp = grid::gpar(fontface = "bold")
        ),
        textGrob("96", x = 1, just = "right"),
        textGrob("115", x = 1, just = "right"),
        textGrob("211",
          x = 1, just = "right",
          gp = grid::gpar(fontface = "bold")
        ),
        textGrob("1673",
          x = 1, just = "right",
          gp = grid::gpar(fontface = "bold")
        )
      ),
    rows = c(1, 5, 6, 8, 11, 12, 14, 16)
  )

  # Data for column 3 (width specified as a physical size below)
  col3 <- list(
    OR = c(0.72, 1.27, 1.17, 2.97, 1.86, 2.01, 1.20),
    LL = c(0.52, 0.87, 1.03, 1.42, 0.46, 1.09, 1.07),
    UL = c(1.00, 1.85, 1.32, 6.21, 7.51, 3.71, 1.35),
    rows = c(5, 6, 8, 11, 12, 14, 16),
    # "s" means summary, "n" means normal
    type = c("n", "n", "s", "n", "n", "s", "s")
  )

  # Sizes of boxes
  information <- sqrt(1 / ((log(col3$UL) - log(col3$OR)) / 1.96))
  col3$sizes <- information / max(information)

  # Width of column 3
  col3width <- grid::unit(1.5, "inches")

  # Range on the x-axis for column 3
  col3$range <- c(0, 4)

  # Draw the table
  #
  # The table is just a big layout
  #
  # All rows are the height of 1 line of text
  #
  # Widths of column 1 and 2 are based on widths of labels in
  # col$labels and col2$labels
  grid::pushViewport(grid::viewport(layout = grid.layout(16, 5,
    widths =
      grid::unit.c(
        max(grid::unit(rep(1, 8), "grobwidth", col1$labels)),
        colgap,
        max(grid::unit(rep(1, 8), "grobwidth", col2$labels)),
        colgap,
        col3width
      ),
    heights = grid::unit(c(1, 0, rep(1, 14)), "lines")
  )))
  grid::pushViewport(grid::viewport(layout.pos.row = 1))
  grid::grid.rect(gp = grid::gpar(col = NA, fill = "black"))
  grid::popViewport()
  for (i in c(8, 14, 16)) {
    grid::pushViewport(grid::viewport(layout.pos.row = i))
    grid::grid.rect(gp = grid::gpar(col = NA, fill = "gray80"))
    grid::popViewport()
  }
  draw_label_col(col1, 1)
  draw_label_col(col1plus, 1)
  draw_label_col(col2, 3)
  draw_data_col(col3, 5)
  grid::popViewport()
  return(0)
}

# Function to draw a cell in a text column
draw_label_col <- function(col, j) {
  for (i in seq_len(length(col$rows))) {
    grid::pushViewport(
      grid::viewport(
        layout.pos.row = col$rows[i],
        layout.pos.col = j
      )
    )
    # Labels are grobs containing their location so just
    # have to grid.draw() them
    grid::grid.draw(col$labels[[i]])
    grid::popViewport()
  }
}

# Function to draw a non-summary rect-plus-CI
draw_normal_ci <- function(ll, or, ul, size) {
  # NOTE the use of "native" units to position relative to
  # the x-axis scale, and "snpc" units to size relative to
  # the height of the row
  # ("snpc" stands for "square normalised parent coordinates"
  #  which means that the value is calculated as a proportion
  #  of the width and height of the current viewport and the
  #  physically smaller of these is used)
  grid.rect(
    x = grid::unit(or, "native"),
    width = grid::unit(size, "snpc"), height = grid::unit(size, "snpc"),
    gp = grid::gpar(fill = "black")
  )
  # Draw arrow if exceed col range
  # convertX() used to convert between coordinate systems
  if (convertX(grid::unit(ul, "native"), "npc", valueOnly = TRUE) > 1) {
    grid::grid.lines(
      x = grid::unit(c(ll, 1), c("native", "npc")), y = .5,
      arrow = arrow(length = grid::unit(0.05, "inches"))
    )
  } else {
    # Draw line white if totally inside rect
    line_col <- if ((convertX(
      grid::unit(or, "native") + grid::unit(
        0.5 * size,
        "lines"
      ),
      "native",
      valueOnly = TRUE
    ) > ul) &&
      (convertX(grid::unit(or, "native") - grid::unit(0.5 * size, "lines"),
        "native",
        valueOnly = TRUE
      ) < ll)) {
      "white"
    } else {
      "black"
    }
    grid::grid.lines(
      x = grid::unit(c(ll, ul), "native"), y = 0.5,
      gp = grid::gpar(col = line_col)
    )
  }
}

# Function to draw a summary "diamond"
draw_summary_ci <- function(ll, or, ul, size) {
  # Not sure how to calc the heights of the diamonds so
  # I'm just using half the height of the equivalent rect
  grid.polygon(
    x = grid::unit(c(ll, or, ul, or), "native"),
    y = grid::unit(0.5 + c(0, 0.25 * size, 0, -0.25 * size), "npc")
  )
}

# Function to draw a "data" column
draw_data_col <- function(col, j) {
  grid::pushViewport(grid::viewport(layout.pos.col = j, xscale = col$range))
  grid::grid.lines(x = grid::unit(1, "native"), y = 0:1)
  # Assume that last value in col is "All"
  grid::grid.lines(
    x = grid::unit(col$OR[length(col$OR)], "native"),
    y = 0:1, gp = grid::gpar(lty = "dashed")
  )
  grid::grid.xaxis(gp = grid::gpar(cex = 0.6))
  grid::grid.text("or", y = grid::unit(-2, "lines"))
  grid::popViewport()
  for (i in seq_len(length(col$rows))) {
    grid::pushViewport(grid::viewport(
      layout.pos.row = col$rows[i], layout.pos.col = j,
      xscale = col$range
    ))
    if (col$type[i] == "n") {
      draw_normal_ci(col$LL[i], col$OR[i], col$UL[i], col$sizes[i])
    } else {
      draw_summary_ci(col$LL[i], col$OR[i], col$UL[i], col$sizes[i])
    }
    grid::popViewport()
  }
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}

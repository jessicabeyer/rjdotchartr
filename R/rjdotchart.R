# rjdotchart
#
#' This is a function which produces dot charts with right-justified y-axis labels
#'
#' @param x either a vector or a matrix of numeric calues (NAs are allowed).
#' @param labels a vector of labels for each point. For vectors the default is to use names(x) and for matrices the row labels dimnames(x)[[1]].
#' @param groups an optional factor indicating how the elements of x are grouped. If x is a matrix, groups will default to the columns of x.
#' @param gdata data values for the groups. This is typically a summary such as the median or mean of each group.
#' @param cex the character size to be used. Setting cex to a value smaller than one can be a useful way of avoiding label overlap. Unlike many other graphics functions, this sets the actual size, not a multiple of par("cex").
#' @param pt.cex the cex to be applied to plotting symbols. This behaves like cex in plot().
#' @param pch the plotting character or symbol to be used
#' @param gpch the plotting character or symbol to be used for group values
#' @param bg the background color of plotting characters or symbols to be used
#' @param color the color(s) to be used for group labels and values
#' @param gcolor the single color to be used for group labels and values
#' @param lcolor the colors to be used for the horizontal lines
#' @param xlim horizontal range for the plot
#' @param main overall title for the plot
#' @param xlab axis notations as in title
#' @param xlab axis notations as in title
#' @return This function is invoked for its side effect, which is to produce two variatns of dotplots, with right-justified y-axis labels
#' @examples
#' rjdotchart(VADeaths, main = "Death Rates in Virginia - 1940")
#' @export
rjdotchart <- function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"),
                       pt.cex = cex, pch = 21, gpch = 21, bg = par("bg"), color = par("fg"),
                       gcolor = par("fg"), lcolor = "gray", xlim = range(x[is.finite(x)]),
                       main = NULL, xlab = NULL, ylab = NULL, ...)
{
  opar <- par("mai", "mar", "cex", "yaxs")
  on.exit(par(opar))
  par(cex = cex, yaxs = "i")
  if (!is.numeric(x))
    stop("'x' must be a numeric vector or matrix")
  n <- length(x)
  if (is.matrix(x)) {
    if (is.null(labels))
      labels <- rownames(x)
    if (is.null(labels))
      labels <- as.character(1L:nrow(x))
    labels <- rep_len(labels, n)
    if (is.null(groups))
      groups <- col(x, as.factor = TRUE)
    glabels <- levels(groups)
  }
  else {
    if (is.null(labels))
      labels <- names(x)
    glabels <- if (!is.null(groups))
      levels(groups)
    if (!is.vector(x)) {
      warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
      x <- as.numeric(x)
    }
  }
  plot.new()
  linch <- if (!is.null(labels))
    max(strwidth(labels, "inch"), na.rm = TRUE)
  else 0
  if (is.null(glabels)) {
    ginch <- 0
    goffset <- 0
  }
  else {
    ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
    goffset <- 0.4
  }
  if (!(is.null(labels) && is.null(glabels))) {
    nmai <- par("mai")
    nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) +
      0.1
    par(mai = nmai)
  }
  if (is.null(groups)) {
    o <- 1L:n
    y <- o
    ylim <- c(0, n + 1)
  }
  else {
    o <- sort.list(as.numeric(groups), decreasing = TRUE)
    x <- x[o]
    groups <- groups[o]
    color <- rep_len(color, length(groups))[o]
    lcolor <- rep_len(lcolor, length(groups))[o]
    offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
    y <- 1L:n + 2 * offset
    ylim <- range(0, y + 2)
  }
  plot.window(xlim = xlim, ylim = ylim, log = "")
  lheight <- par("csi")
  if (!is.null(labels)) {
    linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
    loffset <- (linch )#/lheight
    labs <- labels[o]
    mtext(labs, side = 2, line = loffset, at = y, adj = 1,
          col = color, las = 2, cex = cex, ...)
  }
  abline(h = y, lty = "dotted", col = lcolor)
  points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
  if (!is.null(groups)) {
    gpos <- rev(cumsum(rev(tapply(groups, groups, length)) +
                         2) - 1)
    ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
    goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
    mtext(glabels, side = 2, line = goffset, at = gpos, adj = 1,
          col = gcolor, las = 2, cex = cex, ...)
    if (!is.null(gdata)) {
      abline(h = gpos, lty = "dotted")
      points(gdata, gpos, pch = gpch, col = gcolor, bg = bg,
             cex = pt.cex/cex, ...)
    }
  }
  axis(1)
  box()
  title(main = main, xlab = xlab, ylab = ylab, ...)
  invisible()
}


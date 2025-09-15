### forest plot panels
# panel_boxplot, panel_ci, panel_tplot
###


#' Panel \code{boxplot}
#' 
#' Panel with \code{\link{boxplot}}s.
#' 
#' @param data a list or data frame for each boxplot
#' @param y y-coordinates where each row of \code{data} is plotted
#' @param col color for each, recycled as needed
#' @param axes logical; if \code{TRUE}, the x-axis is drawn
#' @param logx logical; if \code{TRUE}, use a logarithmic x-axis
#' @param add logical; if \code{TRUE}, adds to an existing plot; otherwise, a
#'   new plot is created first
#' @param panel.first,panel.last expressions to be evaluated before and
#'   after any plotting; see \code{\link{plot.default}}
#' @param ... additional arguments passed to \code{\link{boxplot}}
#' 
#' @family panel functions
#' 
#' @examples
#' set.seed(1)
#' l <- replicate(5, rnorm(10), simplify = FALSE)
#' 
#' panel_box(l, add = FALSE, axes = FALSE)
#' 
#' panel_box(l, add = FALSE, axes = FALSE,
#'   panel.first = rect(-1, 0, 1, 6, col = 'grey90', border = NA),
#'   panel.last = abline(v = 0, lwd = 2, col = 'red')
#' )
#' 
#' @export

panel_box <- function(data, y = seq.int(length(data)),
                      col = 1L, axes = FALSE, logx = FALSE, add = TRUE,
                      panel.first = NULL, panel.last = NULL,
                      ...) {
  ## bxp
  z <- boxplot(data, horizontal = TRUE, plot = FALSE)
  xlim <- range(z$stats[is.finite(z$stats)])
  ylim <- range(y, finite = TRUE) + c(-0.5, 0.5)
  
  args <- alist(
    x = as.list(data), horizontal = TRUE, at = y, add = TRUE,
    ylim = ylim, xlim = xlim, frame = FALSE, axes = axes,
    log = ifelse(logx, 'x', '')
  )
  
  if (!add) {
    plot.new()
    plot.window(extendrange(unlist(data)), ylim)
  }
  
  panel.first
  
  op <- par(xpd = NA)
  do.call('boxplot', c(args, ...))
  par(xpd = op$xpd)
  
  panel.last
  
  invisible(NULL)
}

#' Panel CI
#' 
#' Panel with points and confidence intervals.
#' 
#' @param data a data frame or matrix with three columns containing the
#'   point value, lower limit, and upper limit, respectively
#' @param y y-coordinates where each row of \code{data} is plotted
#' @param col,cex,pch color, size, plotting character for each, recycled as
#'   needed
#' @param diamond,cex.diamond logical or indices of rows to use diamonds instead
#'   of \code{pch}; the widths are controlled by the confidence intervals, and
#'   heights are controlled by \code{cex.diamond}
#' @param xlim x-axis limits
#' @param limits limits for intervals; values outside of \code{limits}
#'   will be truncated and drawn as an arrow
#' @param logx logical; if \code{TRUE}, use a logarithmic x-axis
#' @param panel.first,panel.last expressions to be evaluated before and
#'   after any plotting; see \code{\link{plot.default}}
#' @param vline x-position of the vertical dashed line; use \code{NA} for
#'   no line
#' @param type ignored
#' @param ... additional arguments passed to \code{\link{plot.default}}
#' 
#' @family panel functions
#' 
#' @examples
#' dd <- data.frame(
#'   x = 1,
#'   lci = 1 - 1 / 1:10,
#'   uci = 1 + 1 / 1:10
#' )
#' 
#' panel_ci(dd[1:10, ])
#' panel_ci(dd[1:10, ], diamond = c(1, 5, 10))
#' panel_ci(dd[1:10, ], limits = c(0.25, 1.5))
#' panel_ci(dd[1:10, ], limits = c(0.25, 0.95))
#' 
#' @export

panel_ci <- function(data, y = rev(seq.int(nrow(data))),
                     col = 1L, cex = NULL, pch = NULL,
                     diamond = NULL, cex.diamond = 5,
                     xlim = extendrange(unlist(data)), limits = xlim,
                     logx = FALSE, panel.first = NULL, panel.last = NULL,
                     vline = 1, type, ...) {
  # type <- match.arg(type)
  col <- pcol <- rep_len(col, nrow(data))
  
  cex <- if (!is.null(cex))
    rep_len(cex, nrow(data))
  else {
    ok <- any(unlist(data[, 1:3]) < 0, na.rm = TRUE)
    rescaler(
      if (ok)
        abs(data[, 1L])
      else ifelse(abs(data[, 1L]) < 1, 1 / abs(data[, 1L]), abs(data[, 1L])),
      c(1, 5)
      # c(0, max(xx_num, na.rm = TRUE))
    )
  }
  
  dia <- rep_len(FALSE, length(y))
  if (is.function(diamond))
    diamond <- diamond(data)
  
  if (!is.null(diamond)) {
    pcol[diamond] <- NA
    dia[diamond] <- TRUE
  }
  
  plot(data[, 1L], y, ann = FALSE, axes = FALSE, type = 'n', xlim = xlim,
       log = ifelse(logx, 'x', ''), ...,
       panel.first = {
         panel.first
         segments(vline, 0, vline, max(y), lty = 'dashed')
       },
       panel.last = {
         if (TRUE || type) { ## type
           data[, 1L][!data[, 1L] %inside% xlim |
                        !data[, 1L] %inside% limits] <- NA
           points(data[, 1L], y, pch = pch %||% 15L, col = pcol, cex = cex)
           # segments(data[, 2L], y, data[, 3L], yy, col = col)
           
           lo <- data[, 2L]
           hi <- data[, 3L]
           idxl <- lo <= limits[1L]
           idxh <- hi >= limits[2L]
           lo[idxl] <- limits[1L]
           hi[idxh] <- limits[2L]
           
           angles <- cbind(lower = ifelse(idxl, 45, 90),
                           upper = ifelse(idxh, 45, 90))
           angles[is.na(angles)] <- 45
           
           varrows <- Vectorize(arrows, c('code', 'angle'))
           for (ii in seq_along(y)) {
             if (dia[ii]) {
               h <- par('cxy')[2L] / cex.diamond * cex[ii]
               diamond(y[ii], data[ii, 1L], lo[ii], hi[ii], h, col[ii])
               next
             }
             ## zero-length warning
             len <- grconvertX(hi[ii], 'user', 'in') -
               grconvertX(lo[ii], 'user', 'in')
             # if (anyNA(data[ii, -1L]))
             if (is.na(data[ii, 2L]) || is.na(len) || len < 1e-3)
               next
             try({
               varrows(
                 lo[ii], y[ii], hi[ii], y[ii], col = col[ii],
                 code = 1:2, length = 0.05, xpd = NA, angle = angles[ii, ]
               )
             })
           }
         }
         
         panel.last
       })
  
  invisible(NULL)
}

#' Panel \code{tplot}
#' 
#' Panel with \code{\link[rawr]{tplot}}s.
#' 
#' @param data a list or data frame for each tplot
#' @param y y-coordinates where each row of \code{data} is plotted
#' @param col color for each, recycled as needed
#' @param axes logical; if \code{TRUE}, the x-axis is drawn
#' @param logx logical; if \code{TRUE}, use a logarithmic x-axis
#' @param add logical; if \code{TRUE}, adds to an existing plot; otherwise, a
#'   new plot is created first
#' @param panel.first,panel.last expressions to be evaluated before and
#'   after any plotting; see \code{\link{plot.default}}
#' @param ... additional arguments passed to \code{\link{boxplot}}
#' 
#' @family panel functions
#' 
#' @seealso
#' \code{\link[rawr]{tplot}}
#' 
#' @examples
#' \dontrun{
#' library('rawr')
#' set.seed(1)
#' l <- replicate(5, rnorm(10), simplify = FALSE)
#' 
#' panel_tplot(l, add = FALSE)
#' 
#' panel_tplot(l, type = 'd', col = 1:5, pch = 19, add = FALSE,
#'   panel.first = {
#'     p <- par('usr')
#'     rect(-1, p[3L], 1, p[4L], col = 'grey90', border = NA)
#'   },
#'   panel.last = abline(v = 0, lwd = 2, col = 'red')
#' )
#' }
#' 
#' @export

panel_tplot <- function(data, y = seq.int(length(data)), col = 1L,
                        axes = FALSE, logx = FALSE, add = TRUE,
                        panel.first = NULL, panel.last = NULL,
                        ...) {
  stopifnot(is.list(data))
  
  args <- alist(
    x = as.list(data), horizontal = TRUE, at = y, axes = axes,
    ann = FALSE, show.n = FALSE, col = col, add = add,
    panel.first = panel.first, panel.last = panel.last,
    log = ifelse(logx, 'x', '')
  )
  
  do.call('tplot', modifyList(args, list(...)))
  
  invisible(NULL)
}

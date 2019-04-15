### forest plot
# forest
# 
# S3 methods: plot
# plot.forest
# 
# unexported: bars, plot_text, plot.null, vec, vtext
###


#' Forest plot
#' 
#' A forest plot.
#' 
#' @param x an object returned by \code{\link{prepare_list}} or an object of
#' a supported class
#' @param header logical or a vector of character strings used as labels for
#' each variable in the model
#' @param ... additional arguments passed to other methods
#' @param plot logical; if \code{TRUE}, a forest plot is generated; otherwise,
#' a list of data to plot is returned (see \code{plot.forest})
#' @param panel_size proportional size of \code{c(left, middle, right)} panels
#' @param type type of plot for middle panel (currently only \code{"point"}) is
#' supported
#' @param show_conf logical; if \code{TRUE}, the confidence interval is show
#' with the estimate
#' @param xlim the x-axis limits of the plot
#' @param axes logical; if \code{TRUE}, the x-axis is plotted (default); to
#' show a custom axis, set \code{reset_par = FALSE} and use \code{\link{axis}}
#' @param logx logical; if \code{TRUE}, the x-axis will be logarithmic
#' @param reset_par logical; if \code{TRUE}, the graphical parameters are
#' reset to their state before the function call; use \code{FALSE} to
#' continue adding to the middle panel figure
#' @param layout layout of figure, either \code{"split"} where plot splits
#' the text columns or \code{"unified"} where the text columns are adjacent
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' library('survival')
#' lung2 <- within(lung, {
#'   sex <- factor(sex, 1:2, c('Male','Female'))
#'   ph.ecog <- factor(ph.ecog, 0:2)
#' })
#' 
#' x <- coxph(Surv(time, status) ~ age + sex + ph.ecog +
#'              I(meal.cal > 325.5) +
#'              I(wt.loss < 9.5) +
#'              I(ph.karno > 85) +
#'              I(pat.karno > 65), lung2)
#'              
#' x <- forest(x)
#' plot(x, show_conf = TRUE)
#' 
#' palette(c('grey70', 'green4'))
#' plot(x, show_conf = TRUE, cex = 3)
#' palette('default')
#' 
#' 
#' ## equivalent ways to make forest plot
#' x <- coxph(Surv(time, status) ~ age + sex + ph.ecog, lung2)
#' clean <- cleanfp(x)
#' clean_ref <- add_reference(clean)
#' prep_list <- prepare_forest(clean_ref)
#' plot(prep_list)
#' 
#' ## or
#' forest(x, header = c('Age in years', 'Sex', 'ECOG PS'))
#' 
#' 
#' ## plot.forest gives more options
#' plot(prep_list, xlim = c(0, 2.5),
#'      center_panel = {
#'        panel_box(replicate(6, runif(20, 0, 2), simplify = FALSE))
#'        axis(1, mgp = c(3, 2, 1))
#'      })
#' 
#' 
#' fp <- forest(glm(vs ~ factor(gear) + wt + hp, mtcars,
#'                  family = 'binomial'),
#'              plot = FALSE)
#' plot(fp, labels = c(paste('Gear -', 3:5), 'Weight (1k lbs)', 'Horse Power'))
#' 
#' 
#' ## multiple models
#' cx1 <- coxph(Surv(time, status) ~ age + sex + ph.ecog, lung2)
#' cx2 <- coxph(Surv(time, status) ~ age + sex, lung2)
#' cx3 <- coxph(Surv(time, status) ~ age, lung2)
#' 
#' models <- list(cx1, cx2, cx3)
#' prep_lists <- lapply(models, forest, plot = FALSE)
#' prep_lists <- lapply(prep_lists, function(x)
#'   `class<-`(x[[1L]], 'cleanfp_list'))
#' x <- Reduce(merge_forest, prep_lists)
#' 
#' group.col <- rep_len(c('grey95', 'none'), length(models))
#' group.col <- rep(group.col, sapply(prep_lists, function(x) length(x$Term)))
#' plot(x, col.rows = group.col)
#' 
#' 
#' ## competing risks regression
#' library('cmprsk')
#' x <- with(transplant,
#'   crr(futime, as.integer(event) - 1,
#'       cov1 = cbind(age, model.matrix(~ sex + abo)[, -1, drop = FALSE])))
#' forest(x, ~ age + sex + abo, transplant)
#' 
#' 
#' library('cmprsk2')
#' x <- crr2(Surv(futime, event(censored) == death) ~ age + sex + abo, transplant)
#' forest(x)
#' 
#' 
#' ## cox models
#' dat <- within(na.omit(transplant), {
#'   event_ind <- +(event == 'death')
#'   futime <- futime + 1e-8
#' })
#' 
#' 
#' ## coxphf model
#' x <- coxph(Surv(futime, event_ind) ~ age + sex + abo, dat)
#' x <- forest(x, plot = FALSE)
#' plot(x, show_conf = TRUE, xlim = c(0, 5))
#' 
#' 
#' ## coxphf model
#' library('coxphf')
#' x <- coxphf(Surv(futime, event_ind) ~ age + sex + abo, dat)
#' forest(x, data = dat)
#' 
#' @export

forest <- function(x, ..., header = FALSE, plotArgs = list(), plot = TRUE) {
  x <- cleanfp(x, ...)
  x <- add_reference(x, header = header)
  x <- prepare_forest(x)
  
  if (plot)
    do.call('plot', c(list(x = x), plotArgs))
  
  invisible(x)
}

#' @rdname forest
#' @export
plot.forest <- function(x, panel_size = c(0.3, 0.45, 0.25),
                        col.rows, center_panel = NULL, header = FALSE,
                        type = c('ci', 'box', 'tplot'),
                        show_conf = FALSE, labels = NULL,
                        xlim = NULL, axes = TRUE, logx = FALSE,
                        inner.mar = c(0, 0, 0, 0), reset_par = TRUE,
                        layout = c('split', 'unified'), ...) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  layout <- match.arg(layout)
  
  if (inherits(x, c('coxph', ''))) {
    x <- cleanfp(x)
    x <- add_reference(x, header)
    x <- prepare_forest(x)
  }
  
  assert_class(x, 'cleanfp_list')
  ox <- x
  nn <- data.frame(x$numeric)
  
  if (is.null(x$cleanfp_list)) {
    nn <- x$numeric
    x  <- x
  } else {
    x  <- x[[1L]]
    nn <- x$numeric
  }
  nr <- nrow(nn)
  
  if (!is.null(labels)) {
    ii <- if (length(ii <- grep('^  ', x[[1L]]))) {
      labels <- paste0('  ', labels)
      ii
    } else {
      labels <- as.character(labels)
      
      seq.int(nr)
    }
    x[[1L]][ii] <- make.unique(rep_len(labels, nr))[seq_along(ii)]
  }
  
  type <- match.arg(type)
  panel_fn <- switch(
    type,
    ci    = panel_ci,
    box   = panel_box,
    tplot = panel_tplot
  )
  conf.int <- gsub('\\.(\\d+)|.', '\\1', names(nn)[3L])
  
  ## color pvalues < 0.05
  col <- grepl('\\.0[0-4]', x$`p-value`) + 1L
  
  ## identify reference lines
  which_ref <- grep('Reference', x$Estimate)
  
  lx <- seq_along(x[[1L]])
  nx <- length(lx)
  xcf <- cumsum(panel_size)[-length(panel_size)]
  
  plot.new()
  # par(...)
  plot.window(1:2, range(lx, finite = TRUE))
  # m <- matrix(c(1,1,1,2,3,4,5,5,5), 3L, byrow = TRUE)
  # layout(m, heights = c(1, 10, 1))
  # plot.null()
  
  ## base plot
  # plot.null(lx)
  col.rows <- if (missing(col.rows)) {
    grp <- as.integer(ox$cleanfp_ref[[1L]]$group)
    rep(c(grey(.95), NA), length(grp))[grp]
  } else replace(col.rows, col.rows %in% 'none', NA)
  bars(lx, col.rows, TRUE, TRUE)
  
  
  ## left panel
  lp <- x[1:2]
  if (layout == 'split')
    par(fig = c(0, xcf[1L], 0, 1))
  else par(fig = c(0, xcf[1L] * .9, 0, 1))
  # plot.null(lp)
  plot_text(lp, c(1, 2),
            col = vec('black', 'darkgrey', which_ref, nr),
            adj = rep(c(0, 0.5), each = nr),
            # font = vec(1, 3, c(1,5), 10)
            font = 1L
  ) -> at
  vtext(unique(at$x), max(at$y) + c(1, 1), names(lp),
        font = 2, xpd = NA, adj = c(0, 0.5))
  
  
  ## right panel
  rp <- x[c(4:3,3)]
  if (show_conf) {
    rp[[1L]] <- ifelse(
      grepl('Reference', rp[[1L]]), rp[[1L]],
      sprintf('%s (%s, %s)', rp[[1L]], x[[5]][[2L]], x[[5]][[3L]])
    )
    rp[[1L]] <- gsub('(NA.*){3}', '', rp[[1L]])
    names(rp)[1L] <- 'Estimate (LCI, UCI)'
  }
  if (layout == 'split')
    par(fig = c(tail(xcf, -1L), 1, 0, 1))
  else par(fig = c(xcf[1L] * 1.1, xcf[1L] * 1.75, 0, 1))
  
  
  # plot.null(rp)
  plot_text(rp, c(1, 2.5),
            col = c(vec('black', 'darkgrey', which_ref, nr),
                    col, rep('transparent', length(x$Term))),
            font = rep(1L, length(x$Term)),
            adj = rep_len(0.5, length(x$Term))
  ) -> at
  vtext(unique(at$x), max(at$y) + c(1, 1, 1), names(rp),
        font = 2L, xpd = NA, adj = c(NA, NA, 1),
        col = c(palette()[1:2], 'transparent'))
  
  
  ## center panel
  yy <- rev(lx)
  rn <- range(unlist(nn), na.rm = TRUE, finite = TRUE)
  
  ## get xlim if given, force min at 0
  xlim <- xlim %||% c(0, max(rn))
  xlim[1L] <- if (logx)
    1 else 0
  # xlim[1L] <- 0 + pmin(0.1, min(unlist(nn), na.rm = TRUE) * logx)
  
  if (layout == 'split')
    par(
      fig = c(xcf[1L], xcf[2L], 0, 1), new = TRUE, xaxs = 'i',
      mar = pmin(par('mar'), c(NA, 1, NA, NA), na.rm = TRUE) +
        inner.mar + c(0, 0, 0, 1)
    )
  else
    par(
      fig = c(xcf[1L] * 1.5, 1, 0, 1), new = TRUE, xaxs = 'i',
      mar = pmin(par('mar'), c(NA, NA, NA, NA), na.rm = TRUE) +
        inner.mar + c(0, 0, 0, 1)
    )
  plot.new()
  plot.window(xlim, range(lx))
  # plot.null(yy)
  # par(new = TRUE, xaxs = 'i',
  #     mar = pmin(par('mar'), c(NA, 1, NA, NA), na.rm = TRUE) + inner.mar)
  
  if (missing(center_panel)) {
    panel_fn(nn, yy, type = 'n', xlim = xlim, logx = logx, col = col, ...)
    axis(1L)
  } else eval(center_panel)
  
  invisible(op)
}

bars <- function(x, cols = c(grey(.95), NA), horiz = TRUE, fullspan = TRUE) {
  # plot(1:10, type = 'n'); bars(1:10, fullspan = FALSE)
  # plot(1:10, type = 'n'); bars(1:10, c(1,1,2), fullspan = TRUE)
  p <- if (fullspan)
    c(grconvertX(c(0, 1), 'ndc'), grconvertY(c(0, 1), 'ndc'))
  else par('usr')
  
  # cols <- vec(cols[1], cols[2], which(!x %% 2), length(x))
  cols <- rep_len(cols, length(x))
  x <- rev(x) + 0.5
  
  if (horiz)
    rect(p[1L], x - 1L, p[2L], x, border = NA, col = cols, xpd = NA)
  else rect(x - 1L, p[3L], x, p[4L], border = NA, col = rev(cols), xpd = NA)
  
  invisible(NULL)
}

plot_text <- function(x, width = range(seq_along(x)), ...) {
  # plot(col(mtcars), row(mtcars), type = 'n'); forest:::plot_text(mtcars)
  lx <- lengths(x)[1L]
  rn <- range(seq_along(x))
  sx <- (seq_along(x) - 1L) / diff(rn) * diff(width) + width[1]
  xx <- rep(sx, each = lx)
  yy <- rep(rev(seq.int(lx)), length(x))
  vtext(xx, yy, unlist(x), ..., xpd = NA)
  
  invisible(list(x = sx, y = rev(seq.int(lx))))
}

plot.null <- function(window) {
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op))
  try(plot.new())
  if (!missing(window))
    plot.window(1:2, range(window))
  invisible(NULL)
}

vec <- function(default, replacement, idx, n) {
  # vec(1, 0, 2:3, 5); vec(1:5, 0, 2:3)
  res <- if (missing(n))
    default else rep(default, n)
  replace(res, idx, replacement)
}

vtext <- function(...) {
  Vectorize(text.default)(...)
  invisible(NULL)
}

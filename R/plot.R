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
#' @param ... additional arguments passed to other methods or graphical
#' parameters passed to \code{\link{par}}
#' @param header logical or a vector of character strings used as labels for
#' each variable in the model
#' @param plotArgs a named list of additional arguments passed to
#' \code{plot.forest}
#' @param plot logical; if \code{TRUE}, a forest plot is generated; otherwise,
#' a list of data to plot is returned (see \code{plot.forest})
#' @param panel_size proportional size of \code{c(left, middle, right)} panels
#' @param col.rows optional vector of colors for each row
#' @param center_panel (dev) optional function used to draw the plot
#' @param type type of plot for middle panel (currently only \code{"point"}) is
#' supported
#' @param show_percent logical; if \code{TRUE}, percents are shown for each row
#' @param names optional vector of length 4 giving the labels for each column
#' @param show_conf logical; if \code{TRUE}, the confidence interval is show
#' with the estimate
#' @param labels optional vector of labels for each row term
#' @param xlim the x-axis limits of the plot
#' @param axes logical; if \code{TRUE}, the x-axis is plotted (default); to
#' show a custom axis, set \code{reset_par = FALSE} and use \code{\link{axis}}
#' @param logx logical; if \code{TRUE}, the x-axis will be logarithmic
#' @param inner.mar margins for the plot panel
#' @param reset_par logical; if \code{TRUE}, the graphical parameters are
#' reset to their state before the function call; use \code{FALSE} to
#' continue adding to the middle panel figure
#' @param layout layout of figure, either \code{"split"} where plot splits
#' the text columns or \code{"unified"} where the text columns are adjacent
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
#' x <- forest(x, plot = FALSE)
#' plot(x, show_conf = TRUE)
#' 
#' palette(c('grey70', 'green4'))
#' plot(x, show_conf = TRUE, cex = 3)
#' palette('default')
#' 
#' ## add extra columns to left panel
#' plot(x, show_conf = TRUE, panel_size = c(1.5, 1.5, 1), layout = 'unified',
#'      left_panel = list(HR = x$cleanfp_list$numeric$`exp(coef)`))
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
plot.forest <- function(x, panel_size = c(1, 1.5, 0.8), col.rows, at.text = NULL,
                        left_panel = NULL, center_panel = NULL, header = FALSE,
                        type = c('ci', 'box', 'tplot'), show_percent = TRUE,
                        names = NULL, show_conf = FALSE, labels = NULL,
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
  panel_size <- panel_size / sum(panel_size)
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
    rep(c(grey(0.95), NA), length(grp))[grp]
  } else replace(col.rows, col.rows %in% 'none', NA)
  lims <- bars(lx, col.rows, TRUE, TRUE)
  
  
  ## left panel
  lp <- x[c('Term', 'N')]
  lp <- c(lp, left_panel)
  nlp <- seq_along(lp)
  if (show_percent) {
    lp$N <- sprintf('%s (%s)', format(lp$N, big.mark = ','), round(x$P * 100))
    lp$N[grepl('NA', lp$N)] <- ''
    names(lp)[2L] <- 'N (%)'
  }
  
  if (layout == 'split')
    par(fig = c(0, xcf[1L], 0, 1))
  else par(fig = c(0, xcf[1L] * 0.9, 0, 1))
  # plot.null(lp)
  adj <- c(0, rep(0.5, length(lp) - 1L))
  plot_text(
    lp, 1:2, col = vec('black', 'darkgrey', which_ref, nr),
    adj = rep(adj, each = nr), font = 1L, at = at.text[nlp]
  ) -> at
  vtext(
    unique(at$x), max(at$y) + rep_len(1L, length(lp)),
    names[nlp] %||% names(lp), font = 2, xpd = NA, adj = adj
  )
  
  
  ## right panel
  rp <- x[c('Estimate', 'p-value', 'p-value')]
  rp <- c(rp, NULL)
  np <- seq_along(rp)
  if (show_conf) {
    rp$Estimate <- ifelse(
      grepl('Reference', rp[[1L]]), rp[[1L]],
      sprintf('%s (%s, %s)', rp[[1L]], x$text$low, x$text$high)
    )
    rp$Estimate <- gsub('(NA.*){3}', '', rp$Estimate)
    names(rp)[1L] <- 'Estimate (LCI, UCI)'
  }
  if (layout == 'split')
    par(fig = c(tail(xcf, -1L), 1, 0, 1))
  else par(fig = c(xcf[1L] * 1.1, xcf[1L] * 1.75, 0, 1))
  
  # plot.null(rp)
  plot_text(
    rp, c(1, 2.5), at = at.text[-(nlp)],
    col = c(vec('black', 'darkgrey', which_ref, nr),
            col,
            rep('transparent', length(x$Term))),
    font = rep(1L, length(x$Term)), adj = rep_len(0.5, length(x$Term))
  ) -> at
  vtext(
    unique(at$x), max(at$y) + c(1, 1, 1), names[c(1:2, 2) + length(lp)] %||% names(rp),
    font = 2L, xpd = NA, adj = c(NA, NA, 1), col = c(palette()[1:2], 'transparent')
  )
  
  
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
  
  if (missing(center_panel)) {
    panel_fn(nn, yy, type = 'n', xlim = xlim, logx = logx, col = col, ...)
    axis(1L, pos = lims$y[1L])
  } else eval(center_panel)
  
  invisible(op)
}

bars <- function(x, cols = c(grey(.95), NA), horiz = TRUE, fullspan = TRUE) {
  # plot(1:10, type = 'n'); forest:::bars(1:10, fullspan = FALSE)
  # plot(1:10, type = 'n'); forest:::bars(1:10, c(1,1,2), fullspan = TRUE)
  p <- if (fullspan)
    c(grconvertX(c(0, 1), 'ndc'), grconvertY(c(0, 1), 'ndc'))
  else par('usr')
  
  # cols <- vec(cols[1], cols[2], which(!x %% 2), length(x))
  cols <- rep_len(cols, length(x))
  x <- rev(x) + 0.5
  
  if (horiz)
    rect(p[1L], x - 1L, p[2L], x, border = NA, col = cols, xpd = NA)
  else rect(x - 1L, p[3L], x, p[4L], border = NA, col = rev(cols), xpd = NA)
  
  range <- if (horiz)
    list(x = p[1:2], y = range(c(x, x - 1)))
  else list(x = range(c(x, x - 1)), y = p[3:4])
  
  invisible(range)
}

plot_text <- function(x, width = range(seq_along(x)), at = NULL, ...) {
  # plot(col(mtcars), row(mtcars), type = 'n'); forest:::plot_text(mtcars)
  lx <- lengths(x)[1L]
  rn <- range(seq_along(x))
  # sx <- (seq_along(x) - 1L) / diff(rn) * c(1, diff(width)) + width[1L]
  # sx <- cumsum(sapply(head(x, -1), function(x) max(strwidth(x))))
  # sx <- rescaler(c(0, sx), 1:2)
  sx <- c(1, sapply(head(x, -1), function(x) {
    xx <- grep('^\\s+', x, value = TRUE)
    if (!length(xx))
      xx <- x
    max(strwidth(trimws(xx, which = 'right')))
  }))
  sx <- rescaler(cumsum(sx), 1:2)
  if (!is.null(at))
    sx <- grconvertX(at, 'ndc')
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

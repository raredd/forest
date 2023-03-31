
plot2.forest <- function(x, panel_size = c(1, 1.5, 0.8),
                         col.rows = NULL, at.text = NULL,
                         left_panel = NULL, center_panel = NULL, header = FALSE,
                         type = c('ci', 'box', 'tplot'),
                         show_percent = TRUE, show_columns = TRUE,
                         exclude_rows = NULL,
                         names = NULL, show_conf = FALSE, labels = NULL,
                         xlim = NULL, axes = TRUE, logx = FALSE,
                         inner.mar = c(0, 0, 0, 0), reset_par = TRUE,
                         panel.first = NULL, panel.last = NULL,
                         extra_panels = NULL,
                         layout = c('split', 'unified'), ...) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  layout <- if (!is.null(extra_panels))
    'unified' else match.arg(layout)
  
  
  if (inherits(x, c('coxph', ''))) {
    x <- cleanfp(x)
    x <- add_reference(x, header)
    x <- prepare_forest(x)
  }
  
  assert_class(x, 'cleanfp_list')
  
  if (!is.null(exclude_rows)) {
    ## hacky -- be sure to double check
    obj <- x$cleanfp_list
    idx <- grepl(exclude_rows, obj$Term)
    vv <- c('Term', 'N', 'P', 'p-value', 'Estimate')
    obj[vv] <- lapply(obj[vv], function(x) x[!idx])
    obj$text <- lapply(obj$text, function(x) x[!idx])
    obj$numeric <- obj$numeric[!idx, ]
    
    x$cleanfp_ref[[1L]] <- x$cleanfp_ref[[1L]][!idx, ]
    x$cleanfp_ref[[2L]] <- x$cleanfp_ref[[2L]][!idx, ]
    x$cleanfp_list <- obj
  }
  
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
  
  if (is.function(labels)) {
    labfun <- labels
    labels <- NULL
  } else {
    labfun <- identity
  }
  
  if (!is.null(labels)) {
    ii <- if (length(ii <- grep('^  ', x[[1L]]))) {
      labels <- paste0(strrep(' ', 2L), labels)
      ii
    } else {
      labels <- as.character(labels)
      seq.int(nr)
    }
    x[[1L]][ii] <- make.unique(rep_len(labels, nr))[seq_along(ii)]
  } else {
    ## identify and apply function only to row labels (not headers)
    ii <- grep('^  ', x[[1L]])
    lb <- gsub('^  ', '', x[[1L]][ii])
    x[[1L]][ii] <- paste0(strrep(' ', 2L), labfun(lb))
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
  
  ## text columns to show
  show_columns <- rep_len(show_columns, 4L)
  
  lx <- seq_along(x[[1L]])
  nx <- length(lx)
  panel_size <- panel_size / sum(panel_size)
  xcf <- cumsum(panel_size)[-length(panel_size)]
  
  lo <- if (!is.null(extra_panels))
    sequence(3 + length(extra_panels) * 2)
  else switch(layout, split = c(1, 3, 2), unified = 1:3)
  layout(t(lo), widths = panel_size)
  
  plot.new()
  plot.window(1:2, range(lx, finite = TRUE))
  
  panel.first
  
  ## base plot
  col.rows <- if (is.null(col.rows)) {
    grp <- as.integer(ox$cleanfp_ref[[1L]]$group)
    rep(c(grey(0.95), NA), length(grp))[grp]
  } else replace(col.rows, col.rows %in% 'none', NA)
  lims <- bars(lx, col.rows, TRUE, TRUE)
  
  
  ## left panel
  lp <- x[c('Term', 'N')]
  lp[!show_columns[1:2]] <-
    lapply(lp[!show_columns[1:2]], function(x) rep_len(NA, length(x)))
  lp <- c(lp, left_panel)
  nlp <- seq_along(lp)
  if (show_percent) {
    lp$N <- sprintf('%s (%s)', format(lp$N, big.mark = ','), round(x$P * 100))
    lp$N[grepl('NA', lp$N)] <- ''
    names(lp)[2L] <- 'N (%)'
  } else {
    lp$N <- format(lp$N, big.mark = ',')
    lp$N[grepl('NA', lp$N)] <- ''
  }
  
  # plot.null(lp)
  adj <- c(0, rep(0.5, length(lp) - 1L))
  plot_text(
    lp, 1:2, col = vec('black', 'darkgrey', which_ref, nr),
    adj = rep(adj, each = nr), font = 1L, at = at.text[nlp]
  ) -> at
  vtext(
    unique(at$x), max(at$y) + rep_len(1L, length(lp)),
    col = rep_len(
      replace(palette()[c(1L, 1L)], !show_columns[1:2], 'transparent'),
      length(unique(at$x))
    ),
    names[nlp] %||% names(lp), font = 2, xpd = NA, adj = adj
  )
  
  rp_ <- function(x) {
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
    rp[!show_columns[3:4]] <-
      lapply(rp[!show_columns[3:4]], function(x) rep_len(NA, length(x)))
    
    plot.new()
    plot.window(1:2, range(lx, finite = TRUE))
    
    plot_text(
      rp, c(1, 2.5), at = at.text[-(nlp)],
      col = c(vec('black', 'darkgrey', which_ref, nr),
              col,
              rep('transparent', length(x$Term))),
      font = rep(1L, length(x$Term)), adj = rep_len(0.5, length(x$Term))
    ) -> at
    vtext(
      unique(at$x)[1:3], max(at$y) + c(1, 1, 1),
      names[c(1:2, 2) + length(lp)] %||% names(rp),
      font = 2L, xpd = NA, adj = c(NA, NA, 1),
      col = replace(c(palette()[1:2], 'transparent'),
                    !show_columns[c(3, 4, 4)], 'transparent')
    )
  }
  
  cp_ <- function(nn) {
    yy <- rev(lx)
    rn <- range(unlist(nn), na.rm = TRUE, finite = TRUE)
    
    ## get xlim if given, force min at 0
    xlim <- xlim %||% c(0, max(rn))
    # xlim[1L] <- if (logx) 1 else 0
    # xlim[1L] <- 0 + pmin(0.1, min(unlist(nn), na.rm = TRUE) * logx)
    
    par(
      xaxs = 'i',
      mar = pmin(par('mar'), if (layout == 'split')
        c(NA, 1, NA, NA) else c(NA, NA, NA, NA), na.rm = TRUE) +
        inner.mar + c(0, 0, 0, 1)
    )
    
    if (is.null(center_panel)) {
      panel_fn(nn, yy, type = 'n', xlim = xlim, logx = logx, col = col, ...)
      axis(1L, pos = lims$y[1L])
    } else eval(center_panel)
    
    panel.last
  }
  
  ## right panel
  rp_(x)
  
  ## center panel
  cp_(nn)
  
  if (!is.null(extra_panels))
    for (ii in seq_along(extra_panels)) {
      ep <- extra_panels[[ii]]
      rp_(ep$cleanfp_list)
      cp_(ep$cleanfp_list$numeric)
    }
  
  invisible(op)
}

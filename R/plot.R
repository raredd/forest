### forest plot
# forest, forest2, plot.forest
# 
# S3 methods: plot
# plot.forest
# 
# unexported: bars, plot_text, plot.null, vec, vtext
###


#' Forest plot
#' 
#' @description
#' A forest plot.
#' 
#' The typical workflow is to create an object (e.g., \code{glm}, \code{coxph},
#' etc.), clean, and prepare for plot. The final steps may be performed with
#' \code{forest} or \code{forest2}. However, these are pre-defined workflows
#' and may not offer the same level of customization as running each step
#' individually.
#' 
#' For the latter case, the following steps should be followed:
#' \code{\link{cleanfp}}, \code{\link{add_reference}},
#' \code{\link{prepare_forest}}, and \code{plot.forest}; see help pages for
#' each step. User-defined may be substituted for any step as long as the
#' object returned has the proper elements and class.
#' 
#' Alternatively, \code{forest(..., plot = FALSE)} will run the preparation
#' steps and return an object ready for plotting and further customization
#' with the arguments in \code{plot.forest}.
#' 
#' @param x an object returned by \code{\link{prepare_forest}} or an object
#' of a supported class
#' 
#' for \code{forest2}, a (named) list of models to be passed to \code{forest}
#' and aggregated for a single plot
#' 
#' @param ... additional arguments passed to other methods or graphical
#'   parameters passed to \code{\link{par}}
#' @param header logical or a vector of character strings used as labels for
#'   each variable in the model
#'   
#'   for \code{forest2}, a list of header vectors for each model
#' @param font.header,font.labels font style for headers and labels
#' @param total optional argument to give total number of observations, useful
#'   for models which have already removed missing observations; by default,
#'   the percentages will be relative to fitted models rather than total
#'   observations; see examples
#'   
#'   for \code{forest2}, a vector of total observations for each model
#' @param plotArgs a named list of additional arguments passed to
#'   \code{plot.forest}
#' @param plot logical; if \code{TRUE}, a forest plot is generated; otherwise,
#'   a list of data to plot is returned (see \code{plot.forest})
#' @param panel_size proportional size of \code{c(left, middle, right)} panels
#' @param col.rows optional vector of colors for each row background
#' @param at.text optional x-axis locations for the text columns in normalized
#'   device coordinates; see \code{\link{grconvertX}}
#' @param left_panel (dev) a \code{emph} named list of additional text columns
#'   added to the left panel
#' @param center_panel (dev) optional function used to draw the plot
#' @param type type of plot for middle panel (currently only \code{"point"}) is
#'   supported
#' @param space optional indices to insert blank rows
#' @param show_percent logical; if \code{TRUE}, percents are shown for each row
#' @param show_columns logical or a vector of logicals for each text column
#' @param exclude_rows optional pattern to match row labels where any rows
#'   matching will be excluded from the plot
#' @param ref_label label for reference groups; default is \code{"Reference"}
#' @param ref_label_pvalue label for p-value for reference groups; default is
#'   \code{""}
#' @param col.ref,col.header,col.labels vectors of color(s) for the reference,
#'   headers, and row labels
#' @param sig.limit significance limit to highlight plot rows and p-values
#' @param col.sig a vector of colors for \code{>= sig.limit} and
#'   \code{< sig.limit}, respectively, recycled as needed
#' @param names optional vector of length 4 giving the labels for each column
#' @param col.names,font.names color and font vectors for \code{names},
#'   recycled as needed
#' @param show_conf logical; if \code{TRUE}, the confidence interval is show
#'   with the estimate
#' @param conf_format the format for the confidence intervals; default is
#'   \code{"(\%s, \%s)"} with placeholders for the lower and upper limits,
#'   respectively
#' @param labels optional vector of labels for each row term; alternatively, a
#'   function modifying the default generated row labels; see examples
#' @param xlim the x-axis limits of the plot
#' @param axes logical; if \code{TRUE}, the x-axis is plotted (default); to
#'   show a custom axis, set \code{reset_par = FALSE} and use \code{\link{axis}}
#' @param logx logical; if \code{TRUE}, the x-axis will be logarithmic
#' @param inner.mar margins for the plot panel
#' @param reset_par logical; if \code{TRUE}, the graphical parameters are
#'   reset to their state before the function call; use \code{FALSE} to
#'   continue adding to the middle panel figure
#' @param panel.first an expression to be evaluated after the plotting window
#'   has been set up but before any plotting takes place
#' @param panel.last an expression to be evaluated after plotting has taken
#'   place but before exiting the function
#' @param layout layout of figure, either \code{"split"} where plot splits
#'   the text columns or \code{"unified"} where the text columns are adjacent
#' @param order (experimental) manually set order for rows
#' 
#' @seealso
#' \code{\link{summary.forest}}
#' 
#' @examples
#' forest(lm(mpg ~ ., mtcars), plotArgs = list(xlim = c(-5, 5), vline = 0))
#' 
#' library('survival')
#' lung2 <- within(lung, {
#'   sex <- factor(sex, 1:2, c('Male', 'Female'))
#'   ph.ecog <- factor(ph.ecog, 0:2)
#'   age2 <- replace(age, sample(length(age), 50), NA)
#' })
#' 
#' cx <- coxph(Surv(time, status) ~ age + sex + ph.ecog +
#'               I(meal.cal > 325.5) +
#'               I(wt.loss < 9.5) +
#'               I(ph.karno > 85) +
#'               I(pat.karno > 65), lung2)
#'              
#' x <- forest(cx, plot = FALSE)
#' plot(x, show_conf = TRUE)
#' 
#' ## highlighting rows/p-values
#' plot(x, sig.limit = 0.2, col.sig = c('darkgrey', 'red3'))
#' plot(x, col.sig = 'black')
#' 
#' ## change the p-value format
#' x <- forest(cx, plot = FALSE, format_pval = format.pval)
#' plot(x, show_conf = TRUE)
#' 
#' x <- forest(cx, plot = FALSE,
#'   format_pval = function(x) forest:::pvalr(x, digits = 3)
#' )
#' plot(x, show_conf = TRUE)
#' 
#' 
#' \dontrun{
#' ## note that this works but better to use sig.limit/col.sig arguments
#' ## instead to change the color palette
#' palette(c('grey70', 'green4'))
#' plot(x, show_conf = TRUE, cex = 3)
#' 
#' palette(c('black', 'black'))
#' plot(x, show_conf = TRUE, cex = 3)
#' palette('default')
#' }
#' 
#' 
#' ## use a function to modify default row labels
#' plot(x, labels = function(x) gsub('^(sex|I)|[()]|(TRUE|FALSE)', '', x))
#' ## compare to
#' plot(x)
#' 
#' 
#' ## experimental options
#' \dontrun{
#' ## exclude rows without affecting the model
#' plot(x, exclude_rows = 'ecog2')
#' 
#' ## change the order of rows
#' plot(x, order = c(1:3, 5, 4, 6))
#' 
#' ## use diamonds instead of pch
#' ## - width of diamond is the confidence interval
#' plot(x, cex = 1, diamond = 5:6) ## manual
#' plot(x, cex = 1, diamond = function(x) grepl('ecog', x$Term)) ## programmatic
#' 
#' ## add spacing between rows
#' ## - this must be done during the "add_reference" step
#' forest(cx, space = c(2, 4, 7, 9, 11, 13, 15))
#' forest(cx, space = function(x) {
#'   print(names(x)); print(x[, 7]); c(which(!duplicated(x[, 7]))[-1], nrow(x) + 1)
#' })
#' }
#' 
#' ## a useful case for exclude_rows
#' ## note that this no longer applies -- cluster/strata are already excluded
#' ## from the model output, see keep_strata, keep_cluster args from
#' ## forest::add_reference
#' cl <- coxph(Surv(time, status) ~ ph.ecog + sex + cluster(inst), data = lung2)
#' forest(cl)
#' forest(cl, exclude_rows = 'cluster')
#' 
#' 
#' ## add extra columns to left panel
#' plot(
#'   x, show_conf = TRUE, panel_size = c(1.5, 1.5, 1), layout = 'unified',
#'   left_panel = list(HR = x$cleanfp_list$numeric$`exp(coef)`)
#' )
#' 
#' plot(
#'   x, show_conf = TRUE, panel_size = c(2, 1.5, 1), layout = 'unified',
#'   left_panel = list(
#'     HR = x$cleanfp_list$numeric$`exp(coef)`,
#'     ' ' = ifelse(x$cleanfp_list$numeric$p.value < 0.05, '*', '')
#'   )
#' )
#' 
#' 
#' ## use a custom denominator for percents, eg, if the model sample size
#' ## excludes NAs or missing data
#' x <- coxph(Surv(time, status) ~ age2 + sex + ph.ecog, lung2)
#' forest(x, total = nrow(lung2))
#' ## compare
#' forest(x)
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
#' plot(
#'   prep_list,
#'   xlim = c(0, 2),
#'   center_panel = {
#'     panel_box(replicate(6, runif(20, 0, 2), simplify = FALSE))
#'     axis(1, mgp = c(3, 2, 1))
#'   }
#' )
#' 
#' \dontrun{
#' library('rawr') ## for tplot
#' hr_ci <- prep_list$cleanfp_list$numeric[1:3]
#' plot(
#'   prep_list,
#'   xlim = c(0, 4),
#'   center_panel = {
#'     panel_tplot(rev(asplit(hr_ci, 1)), type = 'd', cex = 3,
#'                 pch = c(16, 1, 1), group.pch = FALSE,
#'                 col = c(1, 2, 2), group.col = FALSE)
#'     axis(1, mgp = c(3, 2, 1))
#'   }
#' )
#' }
#' 
#' 
#' ## use forest(..., plot = FALSE) to run steps except plotting which
#' ## can then be customized with plot.forest
#' fp <- forest(glm(vs ~ factor(gear) + wt + hp, mtcars,
#'                  family = 'binomial'),
#'              plot = FALSE)
#' plot(fp, labels = c(paste('Gear -', 3:5), 'Weight (1k lbs)', 'Horse Power'))
#' 
#' 
#' ## multiple models on the same plot
#' models <- list(
#'   'Model 1' = coxph(Surv(time, status) ~ age + sex + ph.ecog, lung2),
#'   'Model 2' = coxph(Surv(time, status) ~ age + sex, lung2),
#'   'Model 3' = coxph(Surv(time, status) ~ age, lung2)
#' )
#' 
#' prep_lists <- lapply(models, function(x) {
#'   x <- forest(x, plot = FALSE)
#'   structure(x[[1L]], class = 'cleanfp_list')
#' })
#' 
#' group.col <- rep_len(c('grey95', 'none'), length(models))
#' group.col <- rep(group.col, sapply(prep_lists, function(x) length(x$Term)))
#' 
#' x <- Reduce(merge_forest, prep_lists)
#' op <- par(no.readonly = TRUE)
#' plot(x, col.rows = group.col, reset_par = FALSE)
#' rl <- rev(rle(group.col)$lengths)
#' yy <- rev(cumsum(head(c(0, rl), -1)) + rl / 2) + 0.5
#' text(grconvertX(0.02, 'ndc'), yy, names(models),
#'      xpd = NA, srt = 90, adj = 0.5)
#' par(op)
#' 
#' ## or simply
#' forest2(models)
#' 
#' ## same but with headers/totals for each model
#' forest2(
#'   models, total = c(230, 240, 250),
#'   labels = function(x) gsub('sex|ph\\.ecog', '', x),
#'   header = list(c('Age', 'Sex', 'ECOG PS'), c('Age', 'Sex'), 'Age')
#' )
#' 
#' 
#' ## other supported objects:
#' ## crr, crr2, coxph, coxphf, logistf, formula
#' 
#' ## competing risks regressions
#' library('cmprsk')
#' x <- with(transplant,
#'   crr(futime, as.integer(event) - 1,
#'       cov1 = cbind(age, model.matrix(~ sex + abo)[, -1, drop = FALSE])))
#' forest(x, ~ age + sex + abo, data = transplant)
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
#' ## survival::coxph model
#' x <- coxph(Surv(futime, event_ind) ~ age + sex + abo, dat)
#' x <- forest(x, plot = FALSE)
#' plot(x, show_conf = TRUE, xlim = c(0, 5))
#' 
#' 
#' ## coxphf::coxphf model
#' library('coxphf')
#' x <- coxphf(Surv(futime, event_ind) ~ age + sex + abo, dat)
#' forest(x, data = dat)
#' 
#' 
#' ## logistf::logistf model
#' library('logistf')
#' x <- logistf(event_ind ~ age + sex + abo, dat)
#' forest(x, data = dat, plotArgs = list(show_conf = TRUE))
#' 
#' 
#' ## brglm2
#' library('brglm2')
#' x <- glm(event_ind ~ age + sex + abo, dat, family = binomial('logit'),
#'          method = 'brglmFit', type = 'AS_mixed')
#' forest(x, plotArgs = list(show_conf = TRUE))
#' 
#' x <- glm(event_ind ~ age + sex + abo, dat, family = binomial('logit'),
#'          method = 'brglmFit', type = 'MPL_Jeffreys')
#' forest(x, plotArgs = list(show_conf = TRUE))
#' 
#' 
#' ## odds ratios/fisher tests
#' dat <- data.frame(
#'   group = c('tx', 'pbo'),
#'   mut = replicate(10, sample(0:1, 50, replace = TRUE))
#' )
#' names(dat)[-1] <- paste0('gene', 1:10)
#' 
#' forest(
#'   group ~ ., dat,
#'   plotArgs = list(xlim = c(0, 20), show_conf = TRUE, cex = 1)
#' )
#' 
#' ## excluding reference level
#' forest(
#'   group ~ ., dat, exclude_rows = '0$',
#'   plotArgs = list(xlim = c(0, 20), show_conf = TRUE, cex = 1)
#' )
#' 
#' @export

forest <- function(x, ..., header = FALSE, total = NULL, exclude_rows = NULL,
                   space = NULL, plotArgs = list(), plot = TRUE) {
  x <- cleanfp(x, ...)
  x <- add_reference(x, header = header, total = total, space = space)
  x <- prepare_forest(x)
  
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
  
  if (plot)
    do.call('plot', c(list(x = x), plotArgs))
  
  invisible(x)
}

order_forest <- function(x, order = NULL) {
  if (is.null(order))
    return(x)
  f2 <- is.null(x$cleanfp_list)
  if (f2)
    x <- list(cleanfp_list = x, cleanfp_ref = NULL)
  
  ## hacky -- be sure to double check
  obj <- x$cleanfp_list
  idx <- seq_along(obj$Term)
  idx <- unique(c(order, idx))
  vv <- c('Term', 'N', 'P', 'p-value', 'Estimate')
  obj[vv] <- lapply(obj[vv], function(x) x[idx])
  obj$text <- lapply(obj$text, function(x) x[idx])
  obj$numeric <- obj$numeric[idx, ]
  
  x$cleanfp_ref[[1L]] <- x$cleanfp_ref[[1L]][idx, ]
  x$cleanfp_ref[[2L]] <- x$cleanfp_ref[[2L]][idx, ]
  x$cleanfp_list <- obj
  
  if (f2)
    x$cleanfp_list else x
}

#' @param formula,data for \code{forest2}, optional arguments to specify a
#'   formula and data set \emph{for each model}; note that this is only
#'   required for some objects; see \code{\link{cleanfp}}
#' @param col.group a vector of colors for each model in \code{x}, recycled
#'   as needed
#' @param groups labels for each model in \code{x}
#' @param FUN (experimental) apply a function to object before plotting
#' 
#' @rdname forest
#' @export

forest2 <- function(x, formula, data, header = FALSE, total = NULL,
                    exclude_rows = NULL, space = NULL, col.group = c('grey95', 'none'),
                    groups = names(x), panel.last = NULL, FUN = NULL, ...) {
  if (!inherits(x, 'list'))
    return(
      forest(x, header = header, total = total, space = space,
                  exclude_rows = exclude_rows, ...)
    )
  
  lx <- length(x)
  if (!missing(formula))
    stopifnot(
      islist(formula),
      length(formula) == lx
    )
  if (!missing(data))
    stopifnot(
      islist(data),
      length(data) == lx
    )
  
  header <- rep_len(header, lx)
  
  if (!is.null(total))
    total <- rep_len(total, lx)
  
  prep_lists <- lapply(seq_along(x), function(ii) {
    x <- forest(
      x[[ii]], formula = formula[[ii]], data = data[[ii]], plot = FALSE,
      space = space,
      header = header[[ii]], total = total[[ii]], exclude_rows = exclude_rows
    )
    structure(x[[1L]], class = 'cleanfp_list')
  })
  
  col.group <- rep_len(col.group, lx)
  col.group <- rep(col.group, sapply(prep_lists, function(x) length(x$Term)))
  
  xx <- Reduce(merge_forest, prep_lists)
  rl <- rev(rle(col.group)$lengths)
  yy <- rev(cumsum(head(c(0, rl), -1)) + rl / 2) + 0.5
  
  if (!is.null(FUN))
    xx <- FUN(xx)
  
  plot(
    xx, col.rows = col.group, ...,
    panel.last = {
      if (!is.null(groups))
        text(grconvertX(0.02, 'ndc'), yy, groups,
             xpd = NA, srt = 90, adj = 0.5)
      panel.last
    }
  )
  
  invisible(xx)
}

#' @rdname forest
#' @export
plot.forest <- function(x, panel_size = c(1, 1.5, 0.8),
                        col.rows = NULL, at.text = NULL,
                        left_panel = NULL, center_panel = NULL,
                        header = FALSE, font.header = 1L, font.labels = 1L,
                        type = c('ci', 'box', 'tplot'), space = NULL,
                        show_percent = TRUE, show_columns = TRUE,
                        exclude_rows = NULL,
                        ref_label = 'Reference', ref_label_pvalue = '',
                        col.ref = 'darkgrey', col.header = 'black', col.labels = 'black',
                        sig.limit = 0.05, col.sig = 1:2,
                        names = NULL, col.names = 'black', font.names = 2L,
                        show_conf = FALSE, conf_format = '(%s, %s)', labels = NULL,
                        xlim = NULL, axes = TRUE, logx = FALSE,
                        inner.mar = c(0, 0, 0, 0), reset_par = TRUE,
                        panel.first = NULL, panel.last = NULL,
                        layout = c('split', 'unified'), order = NULL, ...) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  layout <- match.arg(layout)
  
  if (inherits(x, c('coxph', ''))) {
    x <- cleanfp(x)
    x <- add_reference(x, header = header, space = space)
    x <- prepare_forest(x)
  }
  
  assert_class(x, 'cleanfp_list')
  
  x <- order_forest(x, order)
  
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
  nn$Term <- x$Term
  
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
  # col <- grepl('\\.0[0-4]', x$`p-value`) + 1L
  ## color pvalues < sig.limit
  col.pvalue <- (x$numeric$p.value < sig.limit) + 1L
  col.pvalue <- rep_len(col.sig, 2L)[col.pvalue]
  
  col.names <- rep_len(col.names, 4L)
  font.names <- rep_len(font.names, 4L)
  
  idx.header <- !grepl('^  ', x[[1L]])
  col.header <- rep_len(col.header, sum(idx.header))
  col.labels <- rep_len(col.labels, sum(!idx.header))
  col.text <- rep_len('black', length(idx.header))
  col.text[idx.header] <- col.header
  col.text[!idx.header] <- col.labels
  
  ## identify reference lines
  which_ref <- grep('Reference', rep(x$Estimate, 5L))
  
  ## text columns to show
  show_columns <- rep_len(show_columns, 4L + length(left_panel))
  
  lx <- seq_along(x[[1L]])
  nx <- length(lx)
  
  if (layout == 'unified')
    panel_size <- c(head(panel_size, -2L), rev(tail(panel_size, 2L)))
  panel_size <- panel_size / sum(panel_size)
  xcf <- cumsum(panel_size)[-length(panel_size)]
  
  plot.new()
  # par(...)
  plot.window(1:2, range(lx, finite = TRUE))
  # plot.null()
  
  ## base plot
  # plot.null(lx)
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
  
  if (layout == 'split')
    par(fig = c(0, xcf[1L], 0, 1))
  else par(fig = c(0, xcf[1L] * 0.9, 0, 1))
  # plot.null(lp)
  adj <- c(0, rep(0.5, length(lp) - 1L))
  font <- rep_len(font.labels, length(lp$Term))
  font[!grepl('^\\s', lp$Term)] <- font.header
  plot_text(
    lp, 1:2, col = vec(col.text, col.ref, which_ref, nr, sum(lengths(lp))),
    adj = rep(adj, each = nr), font = font, at = at.text[nlp]
  ) -> at
  vtext(
    at$x[nlp], max(at$y) + rep_len(1L, length(lp)),
    col = rep_len(
      replace(col.names[nlp], !show_columns[nlp], 'transparent'),
      length(at$x[nlp])
    ),
    names[nlp] %||% names(lp),
    font = font.names[rep_len(font.names[nlp], length(unique(at$x)))],
    xpd = NA, adj = adj
  )
  
  
  ## right panel
  rp <- x[c('Estimate', 'p-value', 'p-value')]
  rp <- c(rp, NULL)
  np <- seq_along(rp)
  cf <- paste('%s', conf_format[1L])
  ri <- grepl('Reference', rp[[1L]])
  rp[[2L]][ri] <- rp[[3L]][ri] <- ref_label_pvalue
  if (show_conf) {
    rp$Estimate <- ifelse(
      ri, replace(rp[[1L]], ri, ref_label),
      sprintf(cf, rp[[1L]], x$text$low, x$text$high)
    )
    rp$Estimate <- gsub('(NA.*){3}', '', rp$Estimate)
    names(rp)[1L] <- 'Estimate (LCI, UCI)'
  }
  rp[!show_columns[3:4]] <-
    lapply(rp[!show_columns[3:4]], function(x) rep_len(NA, length(x)))
  
  if (layout == 'split')
    par(fig = c(tail(xcf, -1L), 1, 0, 1))
  else par(fig = c(xcf[1L] * 1.1, xcf[1L] * 1.75, 0, 1))
  
  # plot.null(rp)
  plot_text(
    rp, c(1, 2.5), at = at.text[-(nlp)],
    col = c(vec(col.labels, col.ref, which_ref, nr, sum(lengths(rp[-(1:2)]))),
            replace(col.pvalue, ri, col.ref),
            rep('transparent', length(x$Term))),
    font = rep(1L, length(x$Term)), adj = rep_len(0.5, length(x$Term))
  ) -> at
  vtext(
    unique(at$x)[1:3], max(at$y) + c(1, 1, 1),
    names[c(1:2, 2) + length(lp)] %||% names(rp),
    font = font.names[c(3, 4, 4)], xpd = NA, adj = c(NA, NA, 1),
    col = replace(c(col.names[3:4], 'transparent'),
                  !show_columns[c(3, 4, 4)], 'transparent')
  )
  
  
  ## center panel
  yy <- rev(lx)
  rn <- range(unlist(nn[, 1:3]), na.rm = TRUE, finite = TRUE)
  
  ## get xlim if given, force min at 0
  xlim <- xlim %||% c(0, max(rn))
  # xlim[1L] <- if (logx) 1 else 0
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
  
  if (is.null(center_panel)) {
    panel_fn(nn, yy, type = 'n', xlim = xlim, logx = logx, col = col.pvalue,
             panel.first = panel.first, panel.last = panel.last, ...)
    if (axes)
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

vec <- function(default, replacement, idx, n, n_max = NULL) {
  # vec(1, 0, 2:3, 5); vec(1:5, 0, 2:3)
  res <- if (missing(n))
    default else rep(default, n)
  res <- replace(res, idx, replacement)
  if (!is.null(n_max))
    rep_len(res, n_max) else res
}

vtext <- function(...) {
  Vectorize(text.default)(...)
  invisible(NULL)
}

### forest plot utils
# add_reference, get_n, merge_forest, prepare_forest
# 
# unexported:
# diamond, get_n, insert, locf
# 
# S3 methods: print, summary
# print.forest, summary.forest
###


diamond <- function(y, x, lower, upper, height, col = 'black', ...) {
  # plot(1); diamond(1, 1.2, 1.1, 1.4, 0.1)
  xx <- c(lower, x, upper, x, lower)
  yy <- c(y, y - height / 2, y, y + height / 2, y)
  polygon(xx, yy, border = NA, col = col)
  invisible(list(x = xx, y = yy))
}

get_n <- function(x, percent = FALSE, total = NULL) {
  n <- if (is.numeric(x))
    length(sort(x)) else table(x)
  
  total <- if (is.null(total))
    length(x) else total
  if (percent)
    n / total else n
}

insert <- function(x, where = NULL, what = NA) {
  if (is.null(where))
    return(x)
  if (max(where <- sort(where)) > length(x))
    x <- c(x, rep(NA, max(where) - length(x) - 1L))
  c(x, what)[order(c(seq_along(x), where - 0.5))]
}

locf <- function(x, fromLast = FALSE) {
  # locf(c(1, NA, 2)); locf(c(1, NA, 2), TRUE); locf(c(NA, NA, 2)); locf(c(NA, NA, 2), TRUE)
  if (fromLast)
    return(rev(Recall(rev(x), FALSE)))
  for (ii in seq_along(x))
    x[ii][is.na(x[ii])] <- if (ii == 1L) NA else x[ii - 1L]
  x
}

#' Forest utilities
#' 
#' Not recommended to be called by the user.
#' 
#' @param x,y objects
#' @param header a character vector of header labels for each variable in
#'   the model
#' @param total optional total sample size, useful if model excludes
#'   observations with missing data
#' @param space optional indices to insert blank rows
#' @param keep_strata,keep_cluster logical; if \code{FALSE} (default), strata
#'   and/or cluster variables, e.g., \code{y ~ strata(a) + cluster(b)} will be
#'   omitted from row output
#' 
#' @name forest_utils

#' @rdname forest_utils
#' @export
add_reference <- function(x, header = FALSE, total = NULL, space = NULL,
                          keep_strata = FALSE, keep_cluster = FALSE) {
  assert_class(x, 'cleanfp')
  mf   <- x$model.frame %||% model.frame(x[[2L]])
  nums <- lapply(mf, get_n)
  pcts <- lapply(mf, get_n, total = total, percent = TRUE)
  vars <- colnames(mf)[-1L]
  lbls <- if (is.logical(header))
    NULL else header
  
  pattern <- c('strata\\(', '\\(cluster\\)')[c(!keep_strata, !keep_cluster)]
  if (length(pattern)) {
    pattern <- sprintf('^(%s)', paste0(pattern, collapse = '|'))
    vars <- grep(pattern, vars, invert = TRUE, value = TRUE)
    nums <- nums[grep(pattern, names(nums), invert = TRUE, value = TRUE)]
    pcts <- pcts[grep(pattern, names(pcts), invert = TRUE, value = TRUE)]
  }
  
  ## add suffixes for ordered factors to merge
  ord  <- sapply(mf, is.ordered)
  suf  <- c('', '.L', '.Q', '.C',
            sprintf('^%s', seq.int(pmax(4L, nrow(mf)))))
  
  nn <- lapply(seq_along(nums)[-1L], function(ii)
    paste0(names(nums)[ii],
           if (ord[ii] & grepl('poly', getOption('contrasts')[2L]))
             suf[seq.int(length(nums[[ii]]) - 0L)]
           else names(nums[[ii]]) %||% ''))
  rn <- unlist(nn)
  
  ## create matrix with reference groups to merge
  mm <- matrix(NA, length(rn), 0L, dimnames = list(rn))
  dd <- merge(mm, x[[1L]], by = 0, all = TRUE)
  dd <- dd[match(rownames(mm), dd[, 1L]), ]
  dd$group <- rep(seq_along(nn), lengths(nn))
  if (identical(header, FALSE))
    dd[, 1L] <- paste0('  ', rn)
  
  ref <- is.na(dd[, 2L]) & !trimws(dd[, 1L]) %in% rownames(x[[1L]])
  dd[, 2L][ref] <- 'Reference'
  dd[, ncol(dd)][is.na(dd[, ncol(dd)])] <- '-'
  
  if (!identical(header, FALSE)) {
    nums[] <- Map(c, NA, nums)
    pcts[] <- Map(c, NA, pcts)
    mn <- Map(c, lbls %||% sprintf('header-%s', names(nums)[-1L]), nn)
    rn <- unlist(mn)
    mm <- matrix(NA, length(rn), 0L, dimnames = list(rn))
    dd <- merge(mm, dd, by.x = 0, by.y = 'Row.names', all = TRUE)
    dd <- dd[match(rownames(mm), dd[, 1L]), ]
    dd$group <- rep(seq_along(mn), lengths(mn))
    rn <- Map(c, lbls %||% sprintf('header-%s', names(nums)[-1L]),
              lapply(nn, function(x) paste0('  ', x)))
    dd[, 1L] <- unique(unlist(rn))
    # dd[, 1L] <- unlist(rn)
  }
  
  # Row.names exp.coef. lower..95 upper..95 p.value p.value.numeric group
  if (is.function(space))
    space <- space(dd)
  
  dd <- data.frame(lapply(dd, function(x) insert(x, space)))
  dd$group <- locf(c(1L, dd$group))[-1L]
  
  ## extra data with numeric values
  dd_n <- dd[, c(2:4, 6)]
  suppressWarnings({
    dd_n[] <- lapply(dd_n, as.numeric)
    ## remove p-value formatting (eg, "< 0.01") -- or fix
    # dd_n[] <- lapply(dd_n, function(x) as.numeric(gsub('[< ]', '', x)))
  })
  
  dd$N <- insert(unlist(nums[-1L]), space)
  dd$P <- insert(unlist(pcts[-1L]), space)
  
  rownames(dd) <- rownames(dd_n) <- NULL
  
  structure(
    list(dd, dd_n, object = x$object),
    class = c('forest', 'cleanfp_ref')
  )
}

#' @rdname forest_utils
#' @export
merge_forest <- function(x, y) {
  assert_class(x, 'cleanfp_list')
  if (missing(y))
    return(x)
  assert_class(y, 'cleanfp_list')
  
  ## rawr::clist requires inherits(x, 'list') == TRUE
  class(x) <- class(y) <- 'list'
  
  structure(
    clist(x, y, how = 'rbind'),
    class = c('forest', 'cleanfp_list')
  )
}

#' @rdname forest_utils
#' @export
prepare_forest <- function(x) {
  assert_class(x, 'cleanfp_ref')
  xx <- x[[1L]]
  xn <- x[[2L]]
  
  l <- list(
    Term      = xx$Row.names,
    N         = xx$N,
    P         = xx$P,
    'p-value' = xx$p.value,
    Estimate  = xx[, grep('coef', names(xx))],
    text = list(
      x    = xx[, grep('coef', colnames(xx))],
      low  = xx[, grep('lower', colnames(xx))],
      high = xx[, grep('upper', colnames(xx))]
    ),
    numeric = xn
  )
  
  structure(
    list(cleanfp_list = l, cleanfp_ref = x, object = x$object),
    class = c('forest', 'cleanfp_list')
  )
}

#' @export
print.forest <- function(x, ...) {
  # if (is.null(x$forest_list))
  #   print(x[[1L]]) else print(x)
  
  print(summary(x, html = FALSE))
  
  invisible(x)
}

#' \code{forest} summary
#' 
#' Print a summary of a \code{forest} object.
#' 
#' @param object an object of class \code{\link{forest}}
#' @param html logical; if \code{TRUE}, summary will be formatted for display
#'   output in html; see examples
#' @param ... ignored
#' 
#' @examples
#' x <- forest(
#'   glm(vs ~ factor(gear) + wt + hp, mtcars, family = 'binomial'),
#'   header = c('Gear', 'Weight', 'Horsepower')
#' )
#' summary(x, html = FALSE)
#' 
#' \dontrun{
#' library('htmlTable')
#' s <- summary(x)
#' s <- gsub('factor.*\\)', '', s)
#' htmlTable(s, align = 'lc', caption = 'Model summary.')
#' }
#' 
#' @export

summary.forest <- function(object, html = TRUE, ...) {
  obj <- object$cleanfp_list
  txt <- do.call('cbind', obj$text)
  txt[is.na(txt)] <- ''
  txt[, 3L] <- apply(txt[, -1L], 1, function(x) paste(x, collapse = ' - '))
  txt[, 2L] <- sprintf('%s (%s)', obj$N, round(obj$P * 100))
  txt <- cbind(txt, obj$`p-value`)
  txt[is.na(txt) | txt == ' - ' | txt == 'NA (NA)'] <- ''
  term <- gsub('^ +', if (html) '&emsp;' else ' ', obj$Term)
  colnames(txt) <- c('Estimate', 'N (%)', 'CI', 'p-value')
  txt <- txt[, c(2L, 1L, 3:4)]
  if (html) {
    idx <- !grepl('^&em', term)
    term[idx] <- sprintf('<b>%s</b>', term[idx])
  }
  cbind(Term = term, txt)
}

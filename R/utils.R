### forest plot utils
# add_reference, get_n, merge_forest, prepare_forest
# 
# S3 methods: print
# print.forest
###


#' Forest utilities
#' 
#' Not recommended to be called by the user.
#' 
#' @param x,y objects
#' @param header a character vector of header labels for each variable in
#' the model
#' @param keep_strata logical; if \code{FALSE} (default), strata variables,
#' e.g., \code{y ~ strata(a) + b} will be ignored
#' @param total optional total sample size, useful if model excludes
#' observations with missing data
#' 
#' @name forest_utils

#' @rdname forest_utils
#' @export
add_reference <- function(x, header = FALSE, keep_strata = FALSE, total = NULL) {
  assert_class(x, 'cleanfp')
  mf   <- x$model.frame %||% model.frame(x[[2L]])
  nums <- lapply(mf, get_n)
  pcts <- lapply(mf, get_n, total = total, percent = TRUE)
  vars <- colnames(mf)[-1L]
  lbls <- if (is.logical(header))
    NULL else header
  
  if (!keep_strata) {
    vars <- grep('strata\\(', vars, invert = TRUE, value = TRUE)
    nums <- nums[grep('strata\\(', names(nums), invert = TRUE, value = TRUE)]
    pcts <- pcts[grep('strata\\(', names(pcts), invert = TRUE, value = TRUE)]
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
  
  dd[, 2L][is.na(dd[, 2L])] <- 'Reference'
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
  
  ## extra data with numeric values
  dd_n <- dd[, -c(1L, ncol(dd))]
  suppressWarnings({
    dd_n[] <- lapply(dd_n, as.numeric)
    ## remove p-value formatting (eg, "< 0.01") -- or fix 
    # dd_n[] <- lapply(dd_n, function(x) as.numeric(gsub('[< ]', '', x)))
  })
  
  dd$N <- unlist(nums[-1L])
  dd$P <- unlist(pcts[-1L])
  
  rownames(dd) <- rownames(dd_n) <- NULL
  
  structure(
    list(dd, dd_n, object = x$object),
    class = c('forest', 'cleanfp_ref')
  )
}

get_n <- function(x, percent = FALSE, total = NULL) {
  n <- if (is.numeric(x))
    length(sort(x)) else table(x)
  
  total <- if (is.null(total))
    length(x) else total
  if (percent)
    n / total else n
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

print.forest <- function(x, ...) {
  if (is.null(x$forest_list))
    print(x[[1L]]) else print(x)
  
  invisible(x)
}

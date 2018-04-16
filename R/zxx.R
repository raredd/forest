### internal utils
# %||%, assert_class, clist, pvalr, rescaler, roundr
###


`%||%` <- function(x, y) {
  if (!is.null(x))
    x else y
}

`%inside%` <- function(x, interval) {
  interval <- sort(interval)
  x >= interval[1L] & x <= interval[2L]
}

assert_class <- function(x, class, which = FALSE,
                         message = NULL, warn = FALSE) {
  name <- substitute(x)
  FUN <- if (warn)
    function(...) warning(..., call. = FALSE)
  else function(...) stop(..., call. = FALSE)
  
  if (is.null(message))
    message <- paste(shQuote(name), 'is not of class',
                     toString(shQuote(class)))
  
  if (!all(inherits(x, class, which)))
    FUN(message)
  
  invisible(TRUE)
}

clist <- function (x, y, how = c('cbind', 'rbind')) {
  ## rawr::clist (with modifications)
  if (missing(y))
    return(x)
  
  how <- match.arg(how)
  stopifnot(
    islist(x),
    islist(y)
  )
  
  nn <- names(rapply(c(x, y), names, how = 'list'))
  if (is.null(nn) || any(!nzchar(nn)))
    stop('All non-NULL list elements should have unique names', domain = NA)
  
  nn <- unique(c(names(x), names(y)))
  z  <- setNames(vector('list', length(nn)), nn)
  
  bind <- function(x, y)
    switch(class(x %||% y),
           matrix = match.fun(how),
           data.frame = function(x, y)
             do.call(sprintf('%s.data.frame', how),
                     Filter(Negate(is.null), list(x, y))),
           factor = function(...) unlist(list(...)), c)
  
  for (ii in nn)
    z[[ii]] <- if (islist(x[[ii]]) && islist(y[[ii]]))
      Recall(x[[ii]], y[[ii]]) else
        (bind(x[[ii]], y[[ii]]))(x[[ii]], y[[ii]])
  
  z
}

pvalr <- function(pvals, sig.limit = 0.001, digits = 3L,
         html = FALSE, show.p = FALSE) {
  ## rawr::pvalr
  stopifnot(
    sig.limit > 0,
    sig.limit < 1
  )
  show.p <- show.p + 1L
  html   <- html + 1L
  
  sapply(pvals, function(x, sig.limit) {
    if (is.na(x))
      return(NA)
    if (x >= 0.99)
      return(paste0(c('','p ')[show.p], c('> ','&gt; ')[html], '0.99'))
    if (x >= 0.9)
      return(paste0(c('','p ')[show.p], c('> ','&gt; ')[html], '0.9'))
    if (x < sig.limit) {
      paste0(c('', 'p ')[show.p], c('< ', '&lt; ')[html], format(sig.limit))
    } else {
      nd <- c(digits, 2L, 1L)[findInterval(x, c(-Inf, .1, .5, Inf))]
      paste0(c('','p = ')[show.p], roundr(x, nd))
    }
  }, sig.limit)
}

rescaler <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  ## rawr::rescaler
  zero_range <- function(x, tol = .Machine$double.eps * 100) {
    if (length(x) == 1L)  return(TRUE)
    if (length(x) != 2L)  stop('\'x\' must be length one or two')
    if (any(is.na(x)))    return(NA)
    if (x[1L] == x[2L])   return(TRUE)
    if (all(is.infinite(x))) return(FALSE)
    m <- min(abs(x))
    if (m == 0) return(FALSE)
    abs((x[1L] - x[2L]) / m) < tol
  }
  
  if (zero_range(from) || zero_range(to))
    return(rep(mean(to), length(x)))
  
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}

roundr <- function(x, digits = 1L) {
  ## rawr:::roundr.default (with modifications)
  mode.ok <- vapply(x, function(x)
    is.numeric(x) || is.complex(x) || is.na(x), logical(1L))
  if (!all(mode.ok))
    stop('non-numeric argument to mathematical function')
  
  res <- sprintf(paste0('%.', digits = digits, 'f'), x)
  zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
  res[res == paste0('-', zzz)] <- zzz
  
  setNames(res, names(x))
}

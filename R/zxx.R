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

islist <- function(x) {
  ## is.list(data.frame()); forest:::islist(data.frame())
  inherits(x, 'list')
}

pvalr <- function(pv, sig.limit = 0.001, digits = 2L, scientific = FALSE,
                  html = FALSE, show.p = FALSE, ...) {
  ## rawr::pvalr
  stopifnot(
    sig.limit > 0,
    sig.limit < 1
  )
  
  signif2 <- function(x, digits = 6L) {
    ## rawr:::signif2
    sapply(x, function(xx) {
      s <- signif(xx, digits = digits)
      formatC(s, digits = digits, format = 'fg', flag = '#')
    })
  }
  
  pstr <- c('', 'p ')[show.p + 1L]
  high <- 1 - 1 / 10 ^ digits
  
  sapply(pv, function(x) {
    if (is.na(x) | !nzchar(x))
      NA
    else if (x > high)
      paste0(pstr, c('> ', '&gt; ')[html + 1L], high)
    else if (x < sig.limit)
      paste0(pstr, c('< ', '&lt; ')[html + 1L],
             format.pval(sig.limit, scientific = scientific))
    else paste0(c('', 'p = ')[show.p + 1L], signif2(x, digits))
  })
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

roundr <- function(x, digits = 1L, max = 1e3) {
  ## rawr:::roundr.default (with modifications)
  mode.ok <- vapply(x, function(x)
    is.numeric(x) || is.complex(x) || is.na(x), logical(1L))
  if (!all(mode.ok))
    stop('non-numeric argument to mathematical function')
  
  res <- sprintf(paste0('%.', digits = digits, 'f'), x)
  zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
  res[res == paste0('-', zzz)] <- zzz
  
  ## replace long strings with sci notation
  idx <- abs(x) > max & !is.na(x)
  if (any(idx))
    res[idx] <- format(x, digits = digits)[idx]
  
  setNames(res, names(x))
}

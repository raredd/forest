### as.forest
# as.forest, as.forest.default, as.forest.data.frame
###


#' Coerce objects to forest class
#' 
#' Coerce vectors or data frames to objects for use by \code{plot.forest}.
#' 
#' @param x the model estimates
#' @param lower,upper the lower and upper confidence intervals for \code{x}
#' @param p.value p-values for each estimate
#' @param labels the row labels for each \code{x}
#' @param N,P sample size and percentages for each \code{x}
#' @param digits the number of digits past the decimal point to keep
#' @param ... ignored
#' 
#' @examples
#' x <- as.forest(
#'   x = 1:5, lower = 1:5 - 0.5, upper = 1:5 + 0.5, p.value = runif(5),
#'   labels = paste('var', 1:5), N = 1:5 * 10, P = 1:5 / 10
#' )
#' x
#' plot(x)
#' 
#' 
#' ## with headers
#' x <- c(NA, 1, 2, NA, 1, 2, 3)
#' x <- as.forest(
#'   x = x, lower = x - 1, upper = x + 1, p.value = replace(runif(7), is.na(x), NA),
#'   labels = ifelse(is.na(x), 'header', paste0('   ', x)), N = x * 10, P = x / 10
#' )
#' plot(x, show_conf = TRUE)
#' 
#' 
#' ## example forest plot
#' dat <- data.frame(matrix(rnorm(100 * 8), 100))
#' fit <- glm(I(X1 > 0.5) ~ ., dat, family = 'binomial')
#' forest(fit)
#' 
#' ## compare with as.forest
#' s <- summary(fit)$coefficients[-1L, ]
#' x <- data.frame(exp(s[, 1]), exp(confint(fit))[-1, ], s[, 4])
#' f <- as.forest(
#'   x, labels = gsub('X', 'var ', names(dat)[-1]),
#'   N = colSums(dat[, -1] > 0.5), P = lengths(dat)[-1] / 100
#' )
#' plot(f, col.rows = c('grey95', 'none'))
#' 
#' @export

as.forest <- function(x, ...) {
  UseMethod('as.forest')
}

#' @rdname as.forest
#' @export
as.forest.default <- function(x, lower, upper, p.value, labels, N, P,
                              digits = 2L, ...) {
  n <- length(x)
  
  stopifnot(
    n == length(lower),
    n == length(lower),
    n == length(p.value),
    n == length(labels),
    n == length(N),
    n == length(P)
  )
  
  l <- list(
    Term = labels,
    N = N,
    P = P,
    'p-value' = pvalr(p.value),
    Estimate  = roundr(x, digits),
    text = list(
      x    = roundr(x, digits),
      low  = roundr(lower, digits),
      high = roundr(upper, digits)
    ),
    numeric = data.frame(x, lower, upper, p.value)
  )
  
  structure(
    list(cleanfp_list = l, cleanfp_ref = NULL, object = NULL),
    class = c('forest', 'cleanfp_list')
  )
}

#' @rdname as.forest
#' @export
as.forest.data.frame <- function(x, labels = NULL, N = NULL, P = NULL, ...) {
  nas <- rep_len(NA, nrow(x))
  as.forest.default(
    x = x[, 1L], lower = x[, 2L], upper = x[, 3L], p.value = x[, 4L],
    labels = labels %||% rownames(x), N = N %||% nas, P = P %||% nas
  )
}

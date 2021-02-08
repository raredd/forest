### forest cleaning utils
# objects supported: coxph, coxphf, crr, crr2, glm, logistf
# 
# S3 methods: cleanfp
# cleanfp.default, cleanfp.coxph, cleanfp.coxphf, cleanfp.crr, cleanfp.crr2,
# cleanfp.formula, cleanfp.glm, cleanfp.logistf, cleanfp.table
# 
# unexported:
# clean_fisher, try_fisher
###


#' @export
print.cleanfp <- function(x, ...) {
  print(x$cleanfp)
  invisible(x)
}

#' @export
cleanfp <- function(x, ...) {
  UseMethod('cleanfp')
}

#' @export
cleanfp.default <- function(x, ...) {
  message('Objects of class ', toString(class(x)), ' are not supported')
  invisible(x)
}

#' @export
cleanfp.coxph <- function(x, exp = TRUE, conf.int = 0.95,
                          digits = 2L, format_pval = TRUE) {
  assert_class(x, 'coxph')
  ss <- summary(x)
  co <- ss$coefficients
  ci <- ss$conf.int
  
  res <- cbind(
    data.frame(ci[, -2L, drop = FALSE], check.names = FALSE),
    p.value = co[, grep('^Pr', colnames(co))]
  )
  
  if (!exp) {
    res[, -ncol(res), drop = FALSE] <- log(res[, -ncol(res)])
    names(res)[1L] <- 'coef'
  }
  
  res[] <- lapply(res, roundr, digits = digits)
  
  if (format_pval)
    res$p.value <- pvalr(co[, grep('^Pr', colnames(co))])
  
  structure(
    list(cleanfp = res, object = x, model.frame = model.frame(x)),
    class = c('forest', 'cleanfp')
  )
}

#' @export
cleanfp.crr <- function(x, formula, data, exp = TRUE, conf.int = 0.95,
                        digits = 2L, format_pval = TRUE) {
  assert_class(x, 'crr')
  ss <- summary(x)
  co <- ss$coef
  ci <- ss$conf.int
  colnames(ci)[3:4] <-
    paste0(c('lower .', 'upper .'),
           diff(as.numeric(gsub('%', '', colnames(ci)[3:4], fixed = TRUE))))
  
  res <- cbind(
    data.frame(ci[, -2L, drop = FALSE], check.names = FALSE),
    p.value = co[, grep('^p\\-value', colnames(co))]
  )
  
  if (!exp) {
    res[, -ncol(res), drop = FALSE] <- log(res[, -ncol(res)])
    names(res)[1L] <- 'coef'
  }
  
  res[] <- lapply(res, roundr, digits = digits)
  
  if (format_pval)
    res$p.value <- pvalr(co[, grep('^p\\-value', colnames(co))])
  
  ## need extra things for later objects
  mf <- model.frame(formula, data)
  ## add padding column for add_reference
  mf <- cbind(NA, mf)
  
  structure(
    list(cleanfp = res, object = x, model.frame = mf),
    class = c('forest', 'cleanfp')
  )
}

#' @export
cleanfp.crr2 <- function(x, which = 1L, exp = TRUE, conf.int = 0.95,
                         digits = 2L, format_pval = TRUE) {
  x <- if (any(class(x) %in% 'crr2_list'))
    x[[which]] else {
      assert_class(x, c('crr2', 'crr'))
      x
    }
  mf <- attr(x, 'model.frame')
  
  cleanfp(structure(x, class = 'crr'), reformulate(colnames(mf)),
          mf, exp, conf.int, digits, format_pval)
}

#' @export
cleanfp.coxphf <- function(x, formula = x$call$formula, data,
                           exp = TRUE, conf.int = 1 - x$alpha, digits = 2L,
                           format_pval = TRUE) {
  assert_class(x, c('coxph', 'coxphf'))
  
  capture.output(ss <- summary(x))
  co <- ss$coefficients
  ci <- cbind('exp(coef)' = exp(co), ss$ci.lower, ss$ci.upper)
  colnames(ci)[-1L] <- paste(c('lower', 'upper'), 1 - ss$alpha)
  
  res <- cbind(
    data.frame(ci[, drop = FALSE], check.names = FALSE),
    p.value = ss$prob
  )
  
  if (!exp) {
    res[, -ncol(res), drop = FALSE] <- log(res[, -ncol(res)])
    names(res)[1L] <- 'coef'
  }
  
  res[] <- lapply(res, roundr, digits = digits)
  
  if (format_pval)
    res$p.value <- pvalr(ss$prob)
  
  ## need extra things for later objects
  mf <- model.frame(formula, data)
  
  structure(
    list(cleanfp = res, object = x, model.frame = mf),
    class = c('forest', 'cleanfp')
  )
}

# library('logistf')
# x <- logistf(vs ~ factor(gear) + wt + factor(am), mtcars)
# # forest(x)
# x <- cleanfp(x)
# x <- add_reference(x)
# x <- prepare_forest(x)
# plot(x)

#' @export
cleanfp.logistf <- function(x, exp = TRUE, conf.int = 1 - x$alpha,
                            digits = 2L, format_pval = TRUE) {
  assert_class(x, 'logistf')
  
  capture.output(
    ss <- summary(x)
  )
  co <- ss$coef
  ci <- cbind('exp(coef)' = exp(co), exp(ss$ci.lower), exp(ss$ci.upper))
  colnames(ci)[-1L] <- paste(c('lower', 'upper'), 1 - ss$alpha)
  
  res <- cbind(
    data.frame(ci[, drop = FALSE], check.names = FALSE),
    p.value = ss$prob
  )
  
  if (!exp) {
    res[, -ncol(res), drop = FALSE] <- log(res[, -ncol(res)])
    names(res)[1L] <- 'coef'
  }
  
  res[] <- lapply(res, roundr, digits = digits)
  
  if (format_pval)
    res$p.value <- pvalr(ss$prob)
  
  structure(
    list(cleanfp = res, object = x,
         model.frame = model.frame(x$formula, x$data)),
    class = 'cleanfp'
  )
}

# ## logistic
# x <- glm(vs ~ factor(gear) + wt, mtcars, family = 'binomial')
# forest(x)
# 
# x <- cleanfp(x)
# x <- add_reference(x)
# x <- prepare_forest(x)
# plot(x)

#' @export
cleanfp.glm <- function(x, exp = TRUE, conf.int = 0.95,
                        digits = 2L, format_pval = TRUE) {
  assert_class(x, 'glm')
  stopifnot(x$family$family == 'binomial')
  
  ss <- summary(x)
  co <- ss$coefficients
  suppressMessages(
    ci <- confint(x, level = conf.int)
  )
  colnames(ci) <-
    paste0(c('lower .', 'upper .'), conf.int * 100)
  
  res <- cbind.data.frame(
    coef = exp(co[, 1L]),
    exp(ci),
    p.value = co[, grep('^Pr', colnames(co))]
  )
  
  if (!exp) {
    res[, -ncol(res), drop = FALSE] <- log(res[, -ncol(res)])
    names(res)[1L] <- 'coef'
  }
  
  res[] <- lapply(res, roundr, digits = digits)
  
  if (format_pval)
    res$p.value <- pvalr(co[, grep('^Pr', colnames(co))])
  
  structure(
    list(cleanfp = res, object = x, model.frame = model.frame(x)),
    class = c('forest', 'cleanfp')
  )
}

# x <- table(mtcars$vs, mtcars$am)
# f <- fisher.test(x)
# cleanfp(x)

#' @export
cleanfp.table <- function(x, conf.int = 0.95, digits = 2L,
                          format_pval = TRUE, ...) {
  assert_class(x, 'table')
  
  ft <- try_fisher(x, conf.int = TRUE, conf.level = conf.int, ...)
  
  res <- clean_fisher(ft)
  res[1:3] <- lapply(res[1:3], roundr, digits = digits)
  names(res)[2:3] <- paste0(names(res)[2:3], ' .', round(conf.int * 100))
  
  if (format_pval)
    res$p.value <- pvalr(res$p.value)
  
  structure(
    list(cleanfp = res, object = x, model.frame = x),
    class = c('forest', 'cleanfp')
  )
}


# set.seed(1)
# x <- data.frame(lapply(1:20, function(x) sample(c(0:1, NA), 20, TRUE)))
# names(x) <- letters[seq.int(ncol(x))]
# forest(formula(x), x)
# cleanfp(formula(x), x)

#' @export
cleanfp.formula <- function(formula = formula(data), data, conf.int = 0.95,
                            digits = 2L, format_pval = TRUE,
                            order = c('p.value', 'coef', 'none'),
                            decreasing = FALSE, ...) {
  data[] <- lapply(data, as.factor)
  mf <- model.frame(formula, data, na.action = na.pass)
  y  <- colnames(mf)[1L]
  x  <- colnames(mf)[-1L]
  
  tbl <- lapply(x, function(xx) {
    tt <- table(data[, c(y, xx)])
    ft <- try_fisher(tt, conf.int = TRUE, conf.level = conf.int, ...)
    cbind(clean_fisher(ft), name = paste0(xx, colnames(tt)[-1L]))
  })
  
  res <- do.call('rbind', tbl)
  res[1:3] <- lapply(res[1:3], roundr, digits = digits)
  names(res)[2:3] <- paste0(names(res)[2:3], ' .', round(conf.int * 100))
  rownames(res) <- res$name
  res$name <- NULL
  
  order <- match.arg(order)
  o <- if (order %in% c('coef', 'p.value'))
    order(res[, order], decreasing = decreasing)
  else seq.int(nrow(res))
  
  if (format_pval)
    res$p.value <- pvalr(res$p.value)
  
  ## re-order each object the same
  formula <- reformulate(x[o], y)
  data <- data[, c(1L, o + 1L)]
  res  <- res[o, ]
  mf   <- mf[, c(1L, o + 1L)]
  
  structure(
    list(cleanfp = res, object = list(formula = formula, data = data),
         model.frame = mf, order = o),
    class = c('forest', 'cleanfp')
  )
}

clean_fisher <- function(x) {
  stopifnot(inherits(x, 'htest'))
  
  `%or%` <- function(x, y) {
    unname(if (is.null(x) || !is.finite(x)) y else x)
  }
  
  data.frame(
    coef    = x$estimate %or% NA,
    lower   = x$conf.int[1L] %or% NA,
    upper   = x$conf.int[2L] %or% NA,
    p.value = x$p.value %or% NA
  )
}

try_fisher <- function(x, ...) {
  if (ncol(x) < 2L)
    x <- cbind(x, 0)
  if (nrow(x) < 2L)
    x <- rbind(x, 0)
  
  tryCatch(
    fisher.test(x, ...),
    error = function(e) {
      if (grepl('workspace', e$message)) {
        message('simulating p-value - %s', toString(names(dimnames(x))))
        fisher.test(x, simulate.p.value = TRUE)
      } else e
    }
  )
}

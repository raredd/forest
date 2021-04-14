#' Forest plots
#'
#' Create forest plots from common \code{R} objects.
#' 
#' Currently supported objects:
#' 
#' \tabular{rlll}{
#' \tab \code{\link[survival]{coxph}} \tab Cox proportional hazards regression
#' from the \pkg{survival} package \cr
#' \tab \code{\link[coxphf]{coxphf}} \tab Cox regression with Firth's penalized
#' likelihood from the \pkg{coxphf} package \cr
#' \tab \code{\link[cmprsk]{crr}} \tab Competing risks regression from the
#' \pkg{cmprsk} package \cr
#' \tab \code{\link[cmprsk2]{crr2}} \tab Competing risks regression from the
#' \pkg{cmprsk2} package \cr
#' \tab \code{\link{glm}} \tab Logistic regression \cr
#' \tab \code{\link[logistf]{logistf}} \tab Logistic regression with Firth's
#' penalized likelihood from the \pkg{logistf} package \cr
#' \tab \code{\link{formula}} \tab Fisher's exact tests via
#' \code{\link{fisher.test}} \cr
#' }
#' 
#' @examples
#' twos <- mtcars
#' twos[] <- lapply(twos, function(x) +grepl('1|4', x))
#' forest(mpg ~ ., twos)
#' forest(mpg ~ ., twos, plotArgs = list(xlim = c(0, 20), show_conf = TRUE))
#' 
#' @import graphics grDevices stats utils
#' @name forest-package
#' @docType package
NULL

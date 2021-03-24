#' Standard errors of coefficient estimates
#'
#' @param model an estimated model
#' @param vce a function to compute the covariance matrix of estimates
#' @param ... further arguments to \code{vce}
#'
#' @return a named vector
#' @export
#'
#' @examples
#' data("hprice1")
#'
#' mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)
#' se(mod)
#' # Get heteroskedasticity robust standard errors
#' if (require(sandwich)) {
#'   se(mod, vcovHC)
#' }
#'
se <- function(model, vce = NULL, ...) {
  Vbhat <- patch_vcov(model, vce = vce, ...)
  sqrt(diag(Vbhat))
}

#' Standard errors of coefficient estimates
#'
#' @param model an estimated model
#' @param .vcov a function to compute the covariance matrix of estimates
#' @param ... further arguments to \code{.vcov}
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
se <- function(model, .vcov = NULL, ...) {
  Vbhat <- patch_vcov(model, .vcov = .vcov, ...)
  sqrt(diag(Vbhat))
}

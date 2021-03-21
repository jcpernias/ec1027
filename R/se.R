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
  bhat <- stats::coef(model)
  k <-
  vcov_fn <- stats::vcov
  if(!is.null(.vcov))
    vcov_fn <- .vcov
  Vbhat <- vcov_fn(model, ...)
  if (length(bhat) > NROW(Vbhat))
    Vbhat <- patch_vcov(bhat, Vbhat)
  sqrt(diag(Vbhat))
}

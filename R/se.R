#' Standard errors of coefficient estimates
#'
#' Get the standard error of the estimates of the `model` parameters.
#'
#' The parameter `vce` controls how the covariance matrix of estimates is computed:
#' - If `vce` is `NULL` (the default), the covariance matrix is computed using
#'   [`vcov`][stats::vcov]: `vcov(model)`.
#' - `vce` can be a string that indicates which type of covariance matrix is used.
#'   Covariance matrices robust to heteroskedasticity are computed with `vce = "HC"`.
#'   Other variants valid under heteroskedasticity are "HC0", "HC1", "HC2" and "HC3"
#'   (which is equivalent to "HC"). Newey and West proposed a covariance matrix
#'   estimator valid under autocorrelation and heteroskedasticity. This estimator
#'   is computed by setting `vce`equal to "NW" or "HAC".
#' - `vce` can also be a function. In that case the covariance matrix is
#'   estimated by calling that function: `vce(model)`.
#' - Finally, a covariance matrix can be passed directly to `vce`.
#'
#' The `model` object should support the [`coef`][stats::coef] and
#' [`vcov`][stats::vcov] methods.
#'
#' @param model an estimated model returned by `lm` or similar functions.
#' @param vce an object indicating how to obtain the covariance matrix.
#'
#' @return A named vector with the standard errors of the estimates.
#'
#' @seealso [sandwich::vcovHC], [sandwich::NeweyWest].
#' @export
#'
#' @examples
#' data("hprice1")
#'
#' mod <- lm(price ~ sqrft + bdrms, data = hprice1)
#' se(mod)
#'
#' # Get heteroskedasticity robust standard errors
#' se(mod, vce = "HC")
#'
se <- function(model, vce = NULL) {
  ## Get covariance matrix of estimates
  Vlst <- get_vce(model, vce)

  if (is.null(Vlst$err))
    return(sqrt(diag(Vlst$vce)))
  stop(paste0("Invalid vce argument: ", Vlst$err))
}

#' LM test for heteroskedasticity
#'
#' Fit an auxiliary regression of the squared MCO residual on the covariates
#' specified in `frml`.
#'
#' By default, the test is the F statistic of the auxiliary regression. The LM
#' version of the test is the product of the umber of observations and the
#' R-squared and it is distributed as a \eqn{\chi^2} under the null hypothesis of
#' homoskedasticity.
#'
#' @inheritParams white_test
#' @param frml an one-sided with the regressors of the auxiliary regression.
#'   If `NULL` all of the model regressors are used in the auxiliary regression.
#'
#' @inherit white_test return
#'
#' @examples
#' data("hprice1")
#'
#' mod <- lm(price ~ sqrft + bdrms, data = hprice1)
#'
#' # Test using all regressorn in mod
#' het_test(mod)
#'
#' # Using sqrft only
#' het_test(mod, ~ sqrft)
#'
#' @export
het_test <- function(model, frml = NULL, chisq = FALSE) {
  if(is.null(frml)) {
    Z <- stats::model.matrix(model)
    frml_str <- paste0(" all covariates")
  } else {
    mf <- stats::model.frame(frml, data = model$model)
    Z <- stats::model.matrix(frml, mf)
    frml_str <- paste0(": ", deparse1(frml))
  }
  tst <- het_lm_test(model, Z, chisq)
  tst$method <- "LM test for heteroskedasticity"
  tst$data.name <- paste0("Auxiliary regression of squared residuals from:\n  ",
                          model_call(model), "\non", frml_str, ".\n\n")
  tst
}

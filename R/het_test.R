#' LM test for heteroskedasticity
#'
#' @param model an \code{lm}  object.
#' @param var an one-sided formula for the variance function. If \code{NULL}
#'   the model regressors are used.
#' @param chisq if \code{TRUE} compute the chi-squared statistic. Else compute
#'   the F statistic.
#'
#' @return an object of class \code{htest} with components:
#'   \describe{
#'     \item{statistic}{the value of the test statistic.}
#'     \item{p.value}{the p-value of the test.}
#'     \item{parameter}{degrees of freedom.}
#'     \item{method}{a character string indicating what type of test was
#'       performed.}
#'     \item{data.name}{a character string describing the model.}
#'   }
#'
#'
#' @examples
#' data("hprice1")
#'
#' mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)
#' het_test(mod)
#'
#' @export
het_test <- function(model, var = NULL, chisq = FALSE) {
  if(is.null(var)) {
    Z <- stats::model.matrix(model)
    var_str <- paste0(" all covariates")
  } else {
    mf <- stats::model.frame(var, data = model$model)
    Z <- stats::model.matrix(var, mf)
    var_str <- paste0(": ", deparse1(var))
  }
  tst <- het_lm_test(model, Z, chisq)
  tst$method <- "LM test for heteroskedasticity"
  tst$data.name <- paste0("Auxiliary regression of squared residuals from:\n  ",
                          model_call(model), "\n",
                          "on", var_str, ".\n\n")
  tst
}

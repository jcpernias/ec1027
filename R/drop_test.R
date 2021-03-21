#' Wald test for the exclusion of regressors
#'
#' @param model an \code{lm}  object.
#' @param frml an one-sided formula specifying the terms to be tested.
#'   If \code{NULL} the model regressors are used.
#' @param chisq if \code{TRUE} compute the chi-squared statistic. Else compute
#'   the F statistic.
#' @param .vcov a function computing the variance of the estimates.
#'   If \code{NULL}, \code{vcov} is used.
#' @param ... further parameters passed to \code{.vcov}.
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
#' drop_test(mod)
#'
#' @export
drop_test <- function(model, frml = NULL, .vcov = NULL, ..., chisq = FALSE) {
  .vcov.arg <- substitute(.vcov)
  bhat <- stats::coef(model)
  Vbhat <- patch_vcov(model, bhat = bhat, .vcov = .vcov, ...)

  if (!is.null(frml)) {
    omit <- attr(stats::terms(frml), "term.labels")
    omit_str <- deparse1(frml)
  } else {
    if(attr(stats::terms(model), "intercept") == 0) {
      stop("Model estimated without intercept")
    }
    omit <- names(bhat)[-1]
    omit_str <- "all covariates"
  }
  omit_idx <- as.numeric(names(bhat) %in% omit)
  aliased <- as.numeric(is.na(bhat))
  idx <- omit_idx & !aliased
  k <- sum(idx)
  if(k == 0) {
    stop("No testable restrictions because of aliasing")
  }
  stat <- sum(bhat[idx] * solve(Vbhat[idx, idx], bhat[idx]))
  if (chisq) {
    otest <- chisq_htest(stat, k)
  } else {
    df2 <- stats::df.residual(model)
    otest <- F_htest(stat / k, k, df2)
  }
  otest$method <- "Wald test for redundant variables"
  vcov_str <- ""
  if (!is.null(.vcov.arg)) {
    vcov_str <- paste0("\nCovariance matrix estimate:\n",
                       deparse(.vcov.arg), "(", dots_to_str(...), ")\n")
  }
  otest$data.name <- paste0("Test for redudancy of ", omit_str, ".\n\n",
                            "Call:\n  ", model_call(model), "\n",
                            vcov_str, "\n")
  otest
}

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
#' omit_test(mod)
#'
#' @export
omit_test <- function(model, frml = NULL, chisq = FALSE, .vcov = NULL, ...) {
  bhat <- stats::coef(model)
  if(is.null(.vcov)) {
    .vcov = stats::vcov
  }

  Vbhat <- .vcov(model, ...)
  if(NROW(Vbhat) != length(bhat))
    Vbhat <- patch_vcov(bhat, Vbhat)

  if (!is.null(frml)) {
    omit <- attr(stats::terms(frml), "term.labels")
  } else {
    if(attr(stats::terms(model), "intercept") == 0) {
      stop("Model estimated without intercept")
    }
    omit <- names(bhat)[-1]
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
    otest <- lm_test_chisq(stat, k, "Method", "Data")
  } else {
    df2 <- stats::df.residual(model)
    otest <- lm_test_F(stat / k, k, df2, "Method", "Data")
  }
  otest
}

patch_vcov <- function(bhat, Vbhat) {
  k <- length(bhat)
  bnames <- names(bhat)
  V <- matrix(NA_real_, nrow = k, ncol = k)
  dimnames(V) <- list(bnames, bnames)
  idx <- which(rownames(Vbhat) %in% bnames)
  V[idx, idx] <- Vbhat
  return(V)
}

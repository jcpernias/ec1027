#' White's heteroskedasticity test
#'
#' Compute the heteroskedasticity test proposed by White, H. (1980): "A
#' Heteroskedasticity-Consistent Covariance Matrix Estimator and a Direct
#' Test for Heteroskedasticity". \emph{Econometrica}. \strong{48}(4),
#' 817--838.
#'
#' @inheritParams se
#' @param full if \code{TRUE} add the regressors, their squares and their
#'   cross products to the auxiliary regression. If \code{FALSE} use only the
#'   regressors and their squares.
#' @param chisq if `TRUE` the LM statistic of the auxiliary regression is
#'   returned. By default is `FALSE` and the F statistic is computed.
#'
#' @return An object of class \code{htest} with components:
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
#' white_test(mod)
#'
#' @export
white_test <- function(model, full = TRUE, chisq = FALSE) {
  if (!full)
    var_str <- "all covariates and their squares"
  else
    var_str <- "all covariates, their squares and cross-products"
  Z <- white_test_model_matrix(model, full = full)
  tst <- het_lm_test(model, Z, chisq)
  tst$method <- "White's test for heteroskedasticity"
  tst$data.name <- paste0("Auxiliary regression of squared residuals from:\n  ",
                          model_call(model), "\n",
                          "on ", var_str, ".\n\n")
  tst
}

# Build a matrix with the model regressors, their squares and, opotionally,
# their cross-products
white_test_model_matrix <- function(model, full = TRUE) {
  vars <- stats::model.matrix(model)

  # Do no include the intercept, if there is one
  Xidx <- attr(vars, "assign") != 0
  X <- vars[, Xidx, drop = FALSE]
  sq_X <- X * X

  # Compute cross-products if there are two regressors at least
  k <- ncol(X)
  if (full && k > 1) {
    cross_X <- matrix(nrow = nrow(X), ncol = k * (k - 1) / 2)
    nc <- 1
    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        cross_X[, nc] <- X[, i] * X[, j]
        nc <- nc + 1
      }
    }
    sq_X <- cbind(sq_X, cross_X)
  }
  cbind(vars, sq_X)
}


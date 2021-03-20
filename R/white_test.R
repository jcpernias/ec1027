
lm_test_F <- function(stat, df1, df2, method, data.name) {
  structure(list(statistic = c(F = stat),
                 p.value = stats::pf(stat, df1, df2, lower.tail = FALSE),
                 parameter = c(df1 = df1, df2 = df2),
                 method = method,
                 data.name = data.name),
            class = "htest")
}

lm_test_chisq <- function(chisq, df, method, data.name) {
  structure(list(statistic = c(Chi2 = chisq),
                 p.value = stats::pchisq(chisq, df, lower.tail = FALSE),
                 parameter = c(df = df),
                 method = method,
                 data.name = data.name),
            class = "htest")
}

het_lm_test <- function(model, Z, method, chisq = FALSE) {
  data.name <- substitute(model)
  y <- stats::resid(model)^2
  aux <- stats::.lm.fit(Z, y)

  SST <- sum((y-mean(y))^2)
  SSR <- sum(stats::resid(aux)^2)
  N <- length(y)
  k <- aux$rank - 1
  if(chisq) {
    Rsq <- 1 - SSR / SST
    lm_test <-
      lm_test_chisq(N * Rsq, k,
                    method = method,
                    data.name = data.name)
  } else {
    df2 <- N - k - 1
    Fstat <- ((SST - SSR) / k)/(SSR / df2)
    lm_test <-
      lm_test_F(Fstat, k, df2,
                method = method,
                data.name = data.name)
  }

  lm_test
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
      for(j in (i + 1):k) {
        cross_X[, nc] <- X[, i] * X[, j]
        nc <- nc + 1
      }
    }
    sq_X <- cbind(sq_X, cross_X)
  }
  cbind(vars, sq_X)
}

#' White's heteroskedasticity test
#'
#' Compute the heteroskedasticity test proposed by White, H. (1980): "A
#' Heteroskedasticity-Consistent Covariance Matrix Estimator and a Direct
#' Test for Heteroskedasticity". \emph{Econometrica}. \strong{48}(4),
#' 817--838.
#'
#' @param model an \code{lm}  object.
#' @param full if \code{TRUE} add the regressors, their squares and their
#'   cross products to the auxiliary regression. If \code{FALSE} use only the
#'   regressors and their squares.
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
#' white_test(mod)
#'
#' @export
white_test <- function(model, full = TRUE, chisq = FALSE) {
  method <- "White heteroskedasticity test"
  if (!full)
    method <- paste(method, "(without cross products)")
  Z <- white_test_model_matrix(model, full = full)
  het_lm_test(model, Z, method, chisq)
}

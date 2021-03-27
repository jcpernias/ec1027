#' Print coefficient estimates table
#'
#' @param mod an estimated model
#' @param vce a function to compute the covariance matrix of estimates
#' @param digits number of digits to print
#'
#' @return invisibly returns the coefficient table
#' @export
#'
#' @examples
#' data("hprice1")
#'
#' mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)
#' coef_table(mod)
#'
coef_table <- function(mod, vce = NULL,
                       digits  = max(3L, getOption("digits") - 3L)) {
  vce_arg <- substitute(vce)

  ## Build coeficient table
  bhat <- stats::coef(mod)
  bse <- se(mod, vce = vce)
  tstat <- bhat / bse
  df <- stats::df.residual(mod)
  if (is.null(df)) {
    cnames <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    pval <- 2 * stats::pnorm(abs(tstat), lower.tail = FALSE)
  } else {
    cnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    pval <- 2 * stats::pt(abs(tstat), df = df, lower.tail = FALSE)
  }
  cmat <- cbind(bhat, bse, tstat, pval)
  colnames(cmat) <- cnames

  ## Print summary
  cat("\nCall:\n", paste(model_call(mod), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  if(!is.null(vce)) {
    cat("\nCovariance matrix estimate:\n",
        deparse(vce_arg), "\n",
        sep = "")
  }
  cat("\n")

  aliased <- sum(is.na(bhat))
  k <- length(bhat)
  if (aliased == k) {
    cat("No coefficients\n")
  } else {
    cat("Coefficients: ")
    if (aliased > 0)
      cat("(", aliased, " not defined because of singularities)", sep = "")
    cat("\n")
    stats::printCoefmat(cmat)
  }

  uhat <- stats::resid(mod)
  yhat <- stats::fitted(mod)
  y <- yhat + uhat

  w <- mod$weights
  if (!is.null(w)) {
    SSR <- sum(w * uhat^2)
    SST <- sum(w * (y - sum(w * y) / sum(w))^2)
  } else {
    SSR <- sum(uhat^2)
    SST <- sum((y - mean(y))^2)
  }
  R_sq <- 1 - SSR / SST
  N <- stats::nobs(mod)
  Rbar_sq <- 1 - (SSR / df) / (SST / (N - 1))
  sigma <- sqrt(SSR / df)

  Fstat <- drop_test(mod, vce = vce)

  cat("\nResidual standard error:",
      format(signif(sigma, digits)), "on", df, "degrees of freedom")
  cat("\n")

  na_message <- stats::naprint(mod$na.action)
  if (nzchar(na_message))
    cat("  (", na_message, ")\n", sep = "")

  cat("Multiple R-squared: ", formatC(R_sq, digits = digits))
  cat(",\tAdjusted R-squared: ", formatC(Rbar_sq, digits = digits), "\n")
  cat("F-statistic:", formatC(Fstat$statistic, digits = digits), "on",
      Fstat$parameter[1], "and",  Fstat$parameter[2],
      "DF,  p-value:", format.pval(Fstat$p.value, digits = digits),
      "\n\n")
  invisible(cmat)
}

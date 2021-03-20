#' Print coefficient estimates table
#'
#' @param mod an estimated model
#' @param .vcov a function to compute the covariance matrix of estimates
#' @param ... further arguments to \code{.vcov}
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
coef_table <- function(mod, .vcov = NULL, ...,
                       digits  = max(3L, getOption("digits") - 3L)) {
  .vcov.arg <- substitute(.vcov)
  .dots <- substitute(...())

  ## Build coeficient table
  bhat <- stats::coef(mod)
  bse <- se(mod, .vcov = .vcov, ...)
  tstat <- bhat / bse
  df <- stats::df.residual(mod)
  if (is.null(df)) {
    cnames <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    pval <- stats::pnorm(tstat, lower.tail = FALSE)
  } else {
    cnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    pval <- stats::pt(tstat, df = df, lower.tail = FALSE)
  }
  cmat <- cbind(bhat, bse, tstat, pval)
  colnames(cmat) <- cnames

  ## Print summary
  cat("\nCall:\n", paste(deparse(mod$call), sep = "\n", collapse = "\n"),
      "\n",
      sep = "")
  if(!is.null(.vcov)) {
    if (is.null(.dots)) {
      dots_str <- "()"
    } else {
      dots_str <- deparse1(.dots)
      dots_str <- substr(dots_str, 9, nchar(dots_str))
    }
    cat("\nCovariance matrix estimate:\n",
        paste0(deparse(.vcov.arg)), dots_str, "\n", sep = "")
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

  SSR <- sum(stats::resid(mod)^2)
  yhat <- stats::fitted(mod)
  SSE <- sum((yhat - mean(yhat))^2)
  SST <- SSE + SSR
  R_sq <- 1 - SSR / SST
  Rbar_sq <- 1 - (SSR / df) / (SST / (stats::nobs(mod) - 1))
  sigma <- sqrt(SSR / df)
  Fstat <- omit_test(mod, .vcov = .vcov, ...)

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

#' Print coefficient estimates table
#'
#' Display the output of models estimated with [`lm()`][stats::lm] or similar
#' functions. The parameter `vce` allows to use covariance matrices consistent
#' to heteroskedasticity and autocorrelation (see the documentation of [`se()`][se]).
#'
#' @inheritParams se
#'
#' @return Invisibly returns the coefficient table
#' @export
#'
#' @examples
#' data("hprice1")
#'
#' mod <- lm(price ~ sqrft + bdrms, data = hprice1)
#' coef_table(mod)
#'
#' # Heteroskedasticity consistent standard errors
#' coef_table(mod, vce = "HC")
#'
coef_table <- function(model, vce = NULL) {
  ## Get coefficients
  bhat <- stats::coef(model)

  ## Get covariance matrix of estimates
  Vlst <- get_vce(model, vce)

  if (is.null(Vlst$err)) {
    Vbhat <- Vlst$vce
    vce_msg <- Vlst$msg
  } else {
    stop(paste0("Invalid vce argument: ", Vlst$err))
  }

  ## Build coeficient table
  bse <- sqrt(diag(Vbhat))
  tstat <- bhat / bse
  df <- stats::df.residual(model)
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
  cat("\nCall:\n", paste(model_call(model), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  if(!is.null(vce)) {
    cat("\nCovariance matrix estimate: ",
        vce_msg, ".\n",
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

  uhat <- stats::resid(model)
  yhat <- stats::fitted(model)
  y <- yhat + uhat

  w <- model$weights
  if (!is.null(w)) {
    SSR <- sum(w * uhat^2)
    SST <- sum(w * (y - sum(w * y) / sum(w))^2)
  } else {
    SSR <- sum(uhat^2)
    SST <- sum((y - mean(y))^2)
  }
  R_sq <- 1 - SSR / SST
  N <- stats::nobs(model)
  Rbar_sq <- 1 - (SSR / df) / (SST / (N - 1))
  sigma <- sqrt(SSR / df)

  cat("\nResidual standard error:",
      format(signif(sigma)), "on", df, "degrees of freedom")
  cat("\n")

  na_message <- stats::naprint(model$na.action)
  if (nzchar(na_message))
    cat("  (", na_message, ")\n", sep = "")

  cat("Multiple R-squared: ", formatC(R_sq))
  cat(",\tAdjusted R-squared: ", formatC(Rbar_sq), "\n")

  Fstat <- drop_test(model, vce = Vbhat)
  cat("F-statistic:", formatC(Fstat$statistic), "on",
      Fstat$parameter[1], "and",  Fstat$parameter[2],
      "DF,  p-value:", format.pval(Fstat$p.value),
      "\n\n")
  invisible(cmat)
}

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
  ## Get coefficients
  bhat <- stats::coef(mod)
  ncoef <- length(bhat)

  ## Get covariance matrix of estimates
  if(is.null(vce)) {
    Vbhat <- stats::vcov(mod)
    vce_msg <- "default."
  } else if (is.matrix(vce)) {
    Vbhat <- vce
    vce_msg <- "user supplied matrix."
  } else if (is.function(vce)) {
    Vbhat <- vce(mod)
    vce_msg <- paste0("calling function `", deparse1(substitute(vce)), "`.")
  } else if (is.character(vce)) {
    if(length(vce) != 1)
      stop("Invalid vce argument: character vector of length greater than 1.")
    ## Switch
    Vbhat <- switch(vce,
      "HC" =,
      "HC3" = sandwich::vcovHC(mod, type = "HC3"),
      "HC0" = sandwich::vcovHC(mod, type = "HC0"),
      "HC1" = sandwich::vcovHC(mod, type = "HC1"),
      "HC2" = sandwich::vcovHC(mod, type = "HC2"),
      "NW" =,
      "HAC" = sandwich::NewyWest(mod, prewhite = FALSE),
      stop(paste0("Invalid vce argument: ", vce)))
      vce_msg <- paste0(vce, ".")
  } else {
    ## error
    stop(paste0("Invalid vce argument: ", deparse1(substitute(vce)), "."))
  }


  ## Build coeficient table
  bhat <- stats::coef(mod)
  bse <- sqrt(diag(Vbhat))
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
    cat("\nCovariance matrix estimate: ",
        vce_msg, "\n",
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

  # Fstat <- drop_test(mod, vce = vce)

  cat("\nResidual standard error:",
      format(signif(sigma, digits)), "on", df, "degrees of freedom")
  cat("\n")

  na_message <- stats::naprint(mod$na.action)
  if (nzchar(na_message))
    cat("  (", na_message, ")\n", sep = "")

  cat("Multiple R-squared: ", formatC(R_sq, digits = digits))
  cat(",\tAdjusted R-squared: ", formatC(Rbar_sq, digits = digits), "\n")
  # cat("F-statistic:", formatC(Fstat$statistic, digits = digits), "on",
  #     Fstat$parameter[1], "and",  Fstat$parameter[2],
  #     "DF,  p-value:", format.pval(Fstat$p.value, digits = digits),
  #     "\n\n")
  invisible(cmat)
}

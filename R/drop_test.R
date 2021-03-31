#' Wald test for the exclusion of regressors
#'
#' Test that variables in `frml` can be excluded from `model` using a Wald test.
#' The parameter `vce` determines how is computed the covariance matrix of the
#' model parameters (see [se()][se]).
#'
#' @inheritParams se
#' @param frml an one-sided formula specifying the terms to be tested.
#'   If \code{NULL} all of the model regressors are used.
#' @param chisq if `TRUE` the \eqn{\chi^2} version is returned.
#'   By default is `FALSE` and the F version is computed.
#'
#' @inherit white_test return
#'
#' @examples
#' data("hprice1")
#'
#' mod <- lm(price ~ sqrft + bdrms + colonial, data = hprice1)
#'
#' # Heteroskedasticity-robust joint significance test
#' drop_test(mod, vce = "HC")
#'
#' # Test that bdrms and colonial are redundant (heteroskedasticity robust)
#' drop_test(mod, ~ bdrms + colonial, vce = "HC")
#'
#' @export
drop_test <- function(model, frml = NULL, vce = NULL, chisq = FALSE) {
  bhat <- stats::coef(model)
  bhat_names <- names(bhat)

  ## Get covariance matrix of estimates
  Vlst <- get_vce(model, vce)

  if (is.null(Vlst$err)) {
    Vbhat <- Vlst$vce
    vce_msg <- Vlst$msg
  } else {
    stop(paste0("Invalid vce argument: ", Vlst$err))
  }

  mf <- stats::model.frame(model)
  # Determine if the original model has an intercept
  mt <- attr(mf, "terms")
  has_intercept <- attr(mt, "intercept") != 0

  if (!is.null(frml)) {
    if (length(frml) == 3) {
      warning("frml should be a one-sided formula")
      frml[[2]] <- NULL
    }

    # Ensure that frml only has an intercept if mod has one
    if (has_intercept) {
      frml <- stats::update(frml, ~ . + 1)
    } else {
      frml <- stats::update(frml, ~ . + 0)
    }
    omit <- colnames(stats::model.matrix(frml, data = mf))
    miss <- !omit %in% bhat_names
    if (any(miss)) {
      msg <- paste0("Variables not found: ", paste(omit[miss]), collapse = ", ")
      stop(msg)
    }
    omit_str <- deparse1(frml)
  } else {
    if (!has_intercept) {
      stop("Model estimated without intercept")
    }
    omit <- bhat_names[-1L]
    omit_str <- "all covariates"
  }

  if (length(omit) == 0) {
    stop("No testable restrictions")
  }

  omit_idx <- as.numeric(bhat_names %in% omit)
  aliased <- as.numeric(is.na(bhat))
  idx <- omit_idx & !aliased
  k <- sum(idx)
  if (k == 0) {
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
  vce_str <- ""
  if (!is.null(vce))
    vce_str <- paste0("\nCovariance matrix estimate: ", vce_msg, ".\n")
  otest$data.name <- paste0("Test for redudancy of ", omit_str, ".\n\n",
                            "Call:\n  ", model_call(model), "\n",
                            vce_str, "\n")
  otest
}

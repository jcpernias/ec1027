# Functions in package sandwich drop the rows and columns of aliased
# coefficients. This function adds the corresponding rows and columns
# and it fills them with NAs.
patch_vcov <- function(model, bhat = NULL, vce = NULL, ...) {
  if(is.null(bhat))
    bhat <- stats::coef(model)
  if(is.null(vce))
    vce = stats::vcov
  Vbhat <- vce(model, ...)

  k <- length(bhat)
  if (NROW(Vbhat) == k)
    return(Vbhat)

  V <- matrix(NA_real_, nrow = k, ncol = k)
  bnames <- names(bhat)
  dimnames(V) <- list(bnames, bnames)
  idx <- which(rownames(Vbhat) %in% bnames)
  V[idx, idx] <- Vbhat
  return(V)
}

# Build htest objects
F_htest <- function(stat, df1, df2) {
  structure(list(statistic = c(F = stat),
                 p.value = stats::pf(stat, df1, df2, lower.tail = FALSE),
                 parameter = c(df1 = df1, df2 = df2)),
            class = "htest")
}

chisq_htest <- function(chisq, df) {
  structure(list(statistic = c('X-squared' = chisq),
                 p.value = stats::pchisq(chisq, df, lower.tail = FALSE),
                 parameter = c(df = df)),
            class = "htest")
}

# LM test for heteroskedasticity
het_lm_test <- function(model, Z, chisq = FALSE) {
  name <- substitute(model)
  y <- stats::resid(model)^2
  aux <- stats::.lm.fit(Z, y)

  SST <- sum((y-mean(y))^2)
  SSR <- sum(stats::resid(aux)^2)
  N <- length(y)
  k <- aux$rank - 1
  if(chisq) {
    Rsq <- 1 - SSR / SST
    lm_test <- chisq_htest(N * Rsq, k)
  } else {
    df2 <- N - k - 1
    Fstat <- ((SST - SSR) / k)/(SSR / df2)
    lm_test <- F_htest(Fstat, k, df2)
  }

  lm_test
}

# Get model call
model_call <- function(model) {
  cll <- model$call
  frml <- stats::formula(model)
  cll[[2]] <- frml
  names(cll)[2] <- ""
  deparse1(cll)
}

dots_to_str <- function(...) {
  dots <- substitute(...())
  if (is.null(dots))
    return("")
  str <- deparse1(dots)
  end <- nchar(str) - 1
  start <- nchar(class(dots)) + 2
  substr(deparse1(dots), start, end)
}

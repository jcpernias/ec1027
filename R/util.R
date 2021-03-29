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
  y <- stats::resid(model)^2
  aux <- stats::.lm.fit(Z, y)

  SST <- sum((y - mean(y))^2)
  SSR <- sum(stats::resid(aux)^2)
  N <- length(y)
  k <- aux$rank - 1
  if (chisq) {
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

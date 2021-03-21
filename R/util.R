# Functions in package sandwich drop the rows and columns of aliased
# coefficients. This function adds the corresponding rows and columns
# and it fills them with NAs.
patch_vcov <- function(model, bhat = NULL, .vcov = NULL, ...) {
  if(is.null(bhat))
    bhat <- stats::coef(model)
  if(is.null(.vcov))
    .vcov = stats::vcov
  Vbhat <- .vcov(model, ...)

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

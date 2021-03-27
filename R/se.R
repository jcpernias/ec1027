#' Standard errors of coefficient estimates
#'
#' @param model an estimated model
#' @param vce a function to compute the covariance matrix of estimates
#'
#' @return a named vector
#' @export
#'
#' @examples
#' data("hprice1")
#'
#' mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)
#' se(mod)
#' # Get heteroskedasticity robust standard errors
#' if (require(sandwich)) {
#'   se(mod, vcovHC)
#' }
#'
se <- function(model, vce = NULL) {
  ## Get covariance matrix of estimates
  Vlst <- get_vce(model, vce)

  if (is.null(Vlst$err))
    return(sqrt(diag(Vlst$vce)))
  stop(paste0("Invalid vce argument: ", Vlst$err))
}

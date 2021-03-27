get_vce <- function(mod, how = NULL) {
  err <- NULL
  V <- NULL
  msg <- NULL
  if(is.null(how)) {
    V <- stats::vcov(mod, complete = TRUE)
    msg <- "default"
  } else if (is.matrix(how)) {
    V <- how
    vce_msg <- "user supplied matrix"
  } else if (is.function(how)) {
    V <- how(mod)
    msg <- paste0("calling function `", deparse1(substitute(how)), "`")
  } else if (is.character(how)) {
    if(length(how) != 1) {
      err <- "character vector of length greater than 1"
    } else {
      if (how %in% c("HC", "HC0", "HC1", "HC2", "HC3")) {
        type <- how
        if (type == "HC")
          type <- "HC3"
        V <- sandwich::vcovHC(mod, type = type)
        msg <- how
      } else if (how %in% c("NW", "HAC")) {
        V <- sandwich::NeweyWest(mod, prewhite = FALSE)
        msg <- how
      } else {
        err <- paste0("unknown type `", how, "`")
      }
    }
  } else {
    ## error
    err <- deparse1(substitute(how))
  }

  # Handle aliased coefficients
  aliased <- is.na(stats::coef(mod))
  V <- stats::.vcov.aliased(aliased, V, complete = TRUE)

  list(vce = V, msg = msg, err = err)
}

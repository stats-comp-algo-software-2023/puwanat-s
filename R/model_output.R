#' @export
print.hglm <- function(hglm_out) {
  cat("coefficients:", hglm_out$par, "\nmaximum log-likelihood:", hglm_out$value)
}

#' @export
coef.hglm <- function(hglm_out) {
  return(hglm_out$par)
}

#' @export
vcov.hglm <- function(hglm_out) {
  warning("This function is yet to be implemented.")
}


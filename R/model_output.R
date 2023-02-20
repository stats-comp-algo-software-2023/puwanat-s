#' @export
print.hglm <- function(hglm_out) {
  cat("hiperglm output\n")
}

#' @export
coef.hglm <- function(hglm_out) {

}

#' @export
vcov.hglm <- function(hglm_out) {
  warning("This function is yet to be implemented.")
}


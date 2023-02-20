#' @export
logl_grad <- function(coef, noise_var = 1, y, X) {
  e <- y - (X %*% coef)
  logl_grad_out <- -(t(e) %*% X)/noise_var
  return(logl_grad_out)
}

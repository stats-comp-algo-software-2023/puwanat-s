#' @export
logl_grad <- function(coef, noise_var = 1, y, X) {
  e <- y - (X %*% coef)
  return(-(t(e) %*% X)/noise_var)
}

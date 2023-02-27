#' @export
logl <- function(coef, noise_var = 1, y, X) {
  m <- nrow(X)
  e <- y - (X %*% coef)
  logl_out <- -.5*m*log(2*pi)-.5*m*log(noise_var)-((t(e) %*% e)/(2*noise_var))
  return(-logl_out)
}

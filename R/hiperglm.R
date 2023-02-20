#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()) {
  supported_model <- c("linear", "logit")
  if(!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported", model))
  }
  # MLE finder via pseudo-inverse
  if (length(option) == 0) {
    design.svd <- svd(design)
    design.Pinv <- design.svd$v %*% (1/design.svd$d * t(design.svd$u))
    coef_calc <- design.Pinv %*% outcome
  }
  # MLE finder via BFGS
  else if (option == "BFGS") {

  }

  # TODO: maximize likelihood
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)
}

#' @export
logl <- function(coef, noise_var = 1, y, X) {
  m <- nrow(X)
  e <- y - (X %*% coef)
  logl_out <- -.5*m*log(2*pi)-.5*m*log(noise_var)-((t(e) %*% e)/(2*noise_var))
  return(-logl_out)
}

#' @export
logl_grad <- function(coef, noise_var = 1, y, X) {
  e <- y - (X %*% coef)
  logl_grad_out <- ((t(e) %*% e) %*% X)/noise_var
  return(logl_grad_out)
}
